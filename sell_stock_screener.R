library(quantmod)
library(TTR)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library(progress)

# Function to screen stock and calculate indicators
screen_stock <- function(ticker, custom_date = Sys.Date(), period = "all") {
  tryCatch({
    stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    if (period != "all") {
      end_date <- custom_date
      start_date <- switch(
        period,
        "10m" = end_date - months(10),
        "1y" = end_date - years(1),
        end_date - years(5)
      )

      if (index(stock_data)[1] > start_date) {
        print(paste("Insufficient data for ticker", ticker))
        return(NULL)
      }

      stock_data <- stock_data[paste(start_date, end_date, sep = "::")]
    }

    rsi <- RSI(Cl(stock_data), n = 14)
    macd <- MACD(Cl(stock_data), nFast = 12, nSlow = 26, nSig = 9)
    sma20 <- SMA(Cl(stock_data), n = 20)
    sma50 <- SMA(Cl(stock_data), n = 50)
    sma200 <- SMA(Cl(stock_data), n = 200)
    ema20 <- EMA(Cl(stock_data), n = 20)
    ema50 <- EMA(Cl(stock_data), n = 50)
    ema200 <- EMA(Cl(stock_data), n = 200)

    # New indicators
    atr <- ATR(HLC(stock_data), n = 14)
    roc <- ROC(Cl(stock_data), n = 10)
    bb <- BBands(HLC(stock_data))

    indicators <- data.frame(
      Date = index(stock_data),
      Close = as.numeric(Cl(stock_data)),
      Volume = as.numeric(Vo(stock_data)),
      RSI = as.numeric(rsi),
      MACD = as.numeric(macd[,1]),
      Signal = as.numeric(macd[,2]),
      SMA20 = as.numeric(sma20),
      SMA50 = as.numeric(sma50),
      SMA200 = as.numeric(sma200),
      EMA20 = as.numeric(ema20),
      EMA50 = as.numeric(ema50),
      EMA200 = as.numeric(ema200),
      ATR = as.numeric(atr[,1]),
      ROC = as.numeric(roc),
      BB_Upper = as.numeric(bb[,"up"]),
      BB_Lower = as.numeric(bb[,"dn"])
    )
    return(indicators)

  }, error = function(e) {
    print(paste("Error for ticker", ticker, ":", conditionMessage(e)))
    return(NULL)
  })
}

# Function to check sell signals
check_sell_signals <- function(indicators, lookback_days = 5, rsi_threshold = 70, volume_increase = 1.5) {
  latest_data <- tail(indicators, 1)
  lookback_data <- tail(indicators, lookback_days + 1)

  crossover_occurred <- function(short, long) {
    for (i in 1:lookback_days) {
      if (!is.na(lookback_data[i, short]) && !is.na(lookback_data[i, long]) &&
          !is.na(lookback_data[i + 1, short]) && !is.na(lookback_data[i + 1, long]) &&
          lookback_data[i, short] >= lookback_data[i, long] &&
          lookback_data[i + 1, short] < lookback_data[i + 1, long]) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  macd_sell <- crossover_occurred("Signal", "MACD")
  ema20_sma50_sell <- crossover_occurred("SMA50", "EMA20")
  ema20_sma20_sell <- crossover_occurred("SMA20", "EMA20")
  sma20_sma50_sell <- crossover_occurred("SMA50", "SMA20")

  rsi_sell <- FALSE
  for (i in 1:lookback_days) {
    if (!is.na(lookback_data[i, "RSI"]) && !is.na(lookback_data[i + 1, "RSI"]) &&
        lookback_data[i, "RSI"] > rsi_threshold && lookback_data[i + 1, "RSI"] <= rsi_threshold) {
      rsi_sell <- TRUE
      break
    }
  }

  # Volume confirmation
  volume_confirmed <- latest_data$Volume > (mean(lookback_data$Volume[-(lookback_days + 1)]) * volume_increase)

  # Trend confirmation
  trend_bearish <- latest_data$Close < latest_data$SMA200

  # Momentum check
  momentum_bearish <- latest_data$ROC < 0

  # Volatility check
  volatility_high <- latest_data$ATR > mean(lookback_data$ATR)

  # Bollinger Bands expansion
  bb_expansion <- (latest_data$BB_Upper - latest_data$BB_Lower) >
                  mean(lookback_data$BB_Upper - lookback_data$BB_Lower)

  # Scoring system
  score <- sum(c(
    macd_sell * 3,
    ema20_sma50_sell * 2,
    ema20_sma20_sell * 1,
    sma20_sma50_sell * 1,
    rsi_sell * 1.5,
    volume_confirmed * 2,
    trend_bearish * 3,
    momentum_bearish * 1.5,
    volatility_high * 0.5,
    bb_expansion * 1
  ))

  list(
    MACD_Sell = macd_sell,
    EMA20_SMA50_Sell = ema20_sma50_sell,
    EMA20_SMA20_Sell = ema20_sma20_sell,
    SMA20_SMA50_Sell = sma20_sma50_sell,
    RSI_Sell = rsi_sell,
    Volume_Confirmed = volume_confirmed,
    Trend_Bearish = trend_bearish,
    Momentum_Bearish = momentum_bearish,
    Volatility_High = volatility_high,
    BB_Expansion = bb_expansion,
    Sell_Score = score
  )
}

process_tickers <- function(tickers, period = "all", output_file = "sell_signals.csv", custom_date = Sys.Date(), lookback_period = 3) {
  results <- data.frame(
    Ticker = character(),
    Date = as.Date(character()),
    MACD_Sell = logical(),
    EMA20_SMA50_Sell = logical(),
    EMA20_SMA20_Sell = logical(),
    SMA20_SMA50_Sell = logical(),
    RSI_Sell = logical(),
    Volume_Confirmed = logical(),
    Trend_Bearish = logical(),
    Momentum_Bearish = logical(),
    Volatility_High = logical(),
    BB_Expansion = logical(),
    Sell_Score = numeric(),
    stringsAsFactors = FALSE
  )

  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent in :elapsed",
    total = length(tickers), clear = FALSE, width = 60
  )

  for (ticker in tickers) {
    pb$tick()

    indicators <- screen_stock(ticker, custom_date, period)
    if (is.null(indicators) || nrow(indicators) <= lookback_period) {
      print(paste("Skipping ticker", ticker, "due to insufficient data"))
      next
    }

    signals <- check_sell_signals(indicators, lookback_period)
    latest_data <- tail(indicators, 1)

    signal_data <- data.frame(
      Ticker = ticker,
      Date = latest_data$Date,
      MACD_Sell = signals$MACD_Sell,
      EMA20_SMA50_Sell = signals$EMA20_SMA50_Sell,
      EMA20_SMA20_Sell = signals$EMA20_SMA20_Sell,
      SMA20_SMA50_Sell = signals$SMA20_SMA50_Sell,
      RSI_Sell = signals$RSI_Sell,
      Volume_Confirmed = signals$Volume_Confirmed,
      Trend_Bearish = signals$Trend_Bearish,
      Momentum_Bearish = signals$Momentum_Bearish,
      Volatility_High = signals$Volatility_High,
      BB_Expansion = signals$BB_Expansion,
      Sell_Score = signals$Sell_Score
    )

    results <- rbind(results, signal_data)
  }

  # Sort the results based on the Sell_Score (descending)
  results <- results[order(-results$Sell_Score), ]

  # Write to CSV
  if (file.exists(output_file)) {
    write.table(results, file = output_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    write.table(results, file = output_file, append = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
  }

  return(results)
}

load_tickers <- function(file_path) {
  tryCatch({
    tickers <- read_csv(file_path)

    # Filter out rows where the "SOLD FOR" column is not empty
    tickers <- tickers[is.na(tickers$`SOLD FOR`) | tickers$`SOLD FOR` == "", ]

    # Replace any "^" characters in the Symbol column with "-"
    tickers$Symbol <- gsub("\\^", "-", tickers$Symbol)

    return(tickers$Symbol)
  }, error = function(e) {
    print(paste("Error loading tickers:", conditionMessage(e)))
    return(character(0))
  })
}


# Main execution
current_date <- format(Sys.Date(), "%Y-%m-%d")
num_lookback_days <- 4

output_directory <- paste0("sell_screens/", current_date)
output_file <- paste0(output_directory, "/screen_", current_date, "_lookback-", num_lookback_days, "d.csv")

tickers <- load_tickers("/Users/michaelfelix/Documents/GitHub/rtest/holdings/06-24-2024/2024 - Sheet1.csv")

end_date <- Sys.Date()

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

sell_signals <- process_tickers(tickers, period = "1y", output_file = output_file, custom_date = end_date, lookback_period = num_lookback_days)
