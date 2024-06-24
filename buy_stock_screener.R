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

# Improved check_buy_signals function
check_buy_signals <- function(indicators, lookback_days = 5, rsi_threshold = 30, volume_increase = 1.5) {
  latest_data <- tail(indicators, 1)
  lookback_data <- tail(indicators, lookback_days + 1)

  crossover_occurred <- function(short, long) {
    for (i in 1:lookback_days) {
      if (!is.na(lookback_data[i, short]) && !is.na(lookback_data[i, long]) &&
          !is.na(lookback_data[i + 1, short]) && !is.na(lookback_data[i + 1, long]) &&
          lookback_data[i, short] <= lookback_data[i, long] &&
          lookback_data[i + 1, short] > lookback_data[i + 1, long]) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  macd_buy <- crossover_occurred("MACD", "Signal")
  ema20_sma50_buy <- crossover_occurred("EMA20", "SMA50")
  ema20_sma20_buy <- crossover_occurred("EMA20", "SMA20")
  sma20_sma50_buy <- crossover_occurred("SMA20", "SMA50")

  rsi_buy <- FALSE
  for (i in 1:lookback_days) {
    if (!is.na(lookback_data[i, "RSI"]) && !is.na(lookback_data[i + 1, "RSI"]) &&
        lookback_data[i, "RSI"] < rsi_threshold && lookback_data[i + 1, "RSI"] >= rsi_threshold) {
      rsi_buy <- TRUE
      break
    }
  }

  # Volume confirmation
  volume_confirmed <- latest_data$Volume > (mean(lookback_data$Volume[-(lookback_days + 1)]) * volume_increase)

  # Trend confirmation
  trend_bullish <- latest_data$Close > latest_data$SMA200

  # Momentum check
  momentum_bullish <- latest_data$ROC > 0

  # Volatility check
  volatility_low <- latest_data$ATR < mean(lookback_data$ATR)

  # Bollinger Bands squeeze
  bb_squeeze <- (latest_data$BB_Upper - latest_data$BB_Lower) <
                mean(lookback_data$BB_Upper - lookback_data$BB_Lower)

  # Scoring system
  score <- sum(c(
    macd_buy * 3,          # Increase weight for MACD crossover
    ema20_sma50_buy * 2,   # Increase weight for EMA20/SMA50 crossover
    ema20_sma20_buy * 1,   # Keep weight for EMA20/SMA20 crossover
    sma20_sma50_buy * 1,   # Keep weight for SMA20/SMA50 crossover
    rsi_buy * 1.5,         # Increase weight for RSI crossing threshold
    volume_confirmed * 2,  # Increase weight for volume confirmation
    trend_bullish * 3,     # Increase weight for bullish trend
    momentum_bullish * 1.5, # Increase weight for bullish momentum
    volatility_low * 0.5,  # Keep weight for low volatility
    bb_squeeze * 1         # Keep weight for Bollinger Bands squeeze
  ))

  list(
    MACD_Buy = macd_buy,
    EMA20_SMA50_Buy = ema20_sma50_buy,
    EMA20_SMA20_Buy = ema20_sma20_buy,
    SMA20_SMA50_Buy = sma20_sma50_buy,
    RSI_Buy = rsi_buy,
    Volume_Confirmed = volume_confirmed,
    Trend_Bullish = trend_bullish,
    Momentum_Bullish = momentum_bullish,
    Volatility_Low = volatility_low,
    BB_Squeeze = bb_squeeze,
    Buy_Score = score
  )
}

process_tickers <- function(tickers, period = "all", output_file = "buy_signals.csv", custom_date = Sys.Date(), lookback_period = 3) {
  results <- data.frame(
    Ticker = character(),
    Date = as.Date(character()),
    MACD_Buy = logical(),
    EMA20_SMA50_Buy = logical(),
    EMA20_SMA20_Buy = logical(),
    SMA20_SMA50_Buy = logical(),
    RSI_Buy = logical(),
    Volume_Confirmed = logical(),
    Trend_Bullish = logical(),
    Momentum_Bullish = logical(),
    Volatility_Low = logical(),
    BB_Squeeze = logical(),
    Buy_Score = numeric(),
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

    signals <- check_buy_signals(indicators, lookback_period)
    latest_data <- tail(indicators, 1)

    signal_data <- data.frame(
      Ticker = ticker,
      Date = latest_data$Date,
      MACD_Buy = signals$MACD_Buy,
      EMA20_SMA50_Buy = signals$EMA20_SMA50_Buy,
      EMA20_SMA20_Buy = signals$EMA20_SMA20_Buy,
      SMA20_SMA50_Buy = signals$SMA20_SMA50_Buy,
      RSI_Buy = signals$RSI_Buy,
      Volume_Confirmed = signals$Volume_Confirmed,
      Trend_Bullish = signals$Trend_Bullish,
      Momentum_Bullish = signals$Momentum_Bullish,
      Volatility_Low = signals$Volatility_Low,
      BB_Squeeze = signals$BB_Squeeze,
      Buy_Score = signals$Buy_Score
    )

    results <- rbind(results, signal_data)
  }

  # Sort the results based on the Buy_Score (descending)
  results <- results[order(-results$Buy_Score), ]

  # Write to CSV
  if (file.exists(output_file)) {
    write.table(results, file = output_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    write.table(results, file = output_file, append = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
  }

  return(results)
}

# The rest of the code remains the same
load_tickers <- function(file_path) {
  tickers <- read_csv(file_path)
  tickers$Symbol <- gsub("\\^", "-", tickers$Symbol)
  return(tickers$Symbol)
}

load_buy_tickers <- function(file_path) {
  tickers <- read_csv(file_path)
  return(tickers$Ticker)
}

current_date <- format(Sys.Date(), "%Y-%m-%d")

num_lookback_days <- 4

output_directory <- paste0("buy_screens/", current_date)
output_file <- paste0(output_directory, "/screen_", current_date, "_lookback-", num_lookback_days, "d.csv")

tickers <- load_tickers("/Users/michaelfelix/Documents/GitHub/rtest/tickers.csv")
#tickers <- load_buy_tickers("/Users/michaelfelix/Documents/GitHub/rtest/buy_signals_2024-06-19.csv")

#end_date <- as.Date("2024-05-22")
end_date <- Sys.Date()

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

buy_signals <- process_tickers(tickers, period = "1y", output_file = output_file, custom_date = end_date, lookback_period = num_lookback_days)
