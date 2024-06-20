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

      # Check if data is available for more than 1 year
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
      EMA200 = as.numeric(ema200)
    )
    return(indicators)

  }, error = function(e) {
    print(paste("Error for ticker", ticker, ":", conditionMessage(e)))
    return(NULL)
  })
}

# Improved check_buy_signals function
check_buy_signals <- function(indicators, lookback_days = 5, rsi_threshold = 30, volume_factor = 1.5) {
  latest_data <- tail(indicators, 1)
  lookback_data <- tail(indicators, lookback_days + 1)

  # Helper function to check if a crossover occurred within the last 'lookback_days'
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

  # MACD buy signal: crossover occurred within the lookback period
  macd_buy <- crossover_occurred("MACD", "Signal")

  # EMA20 > SMA50 crossover: occurred within the lookback period
  ema20_sma50_buy <- crossover_occurred("EMA20", "SMA50")

  # EMA20 > SMA20 crossover: occurred within the lookback period
  ema20_sma20_buy <- crossover_occurred("EMA20", "SMA20")

  # SMA20 > SMA50 crossover: occurred within the lookback period
  sma20_sma50_buy <- crossover_occurred("SMA20", "SMA50")

  # RSI buy signal: RSI dropped below threshold and then risen above it within the lookback period
  rsi_buy <- FALSE
  for (i in 1:lookback_days) {
    if (!is.na(lookback_data[i, "RSI"]) && !is.na(lookback_data[i + 1, "RSI"]) &&
        lookback_data[i, "RSI"] < rsi_threshold && lookback_data[i + 1, "RSI"] >= rsi_threshold) {
      rsi_buy <- TRUE
      break
    }
  }

  # Add Bollinger Bands
  bb <- BBands(indicators$Close, n = 20, sd = 2)
  bb_buy <- latest_data$Close < bb[,"dn"]  # Price below lower Bollinger Band
  # Volume confirmation
  volume_buy <- !is.na(latest_data$Volume) &&
                !is.na(mean(tail(indicators$Volume, 20), na.rm = TRUE)) &&
                latest_data$Volume > (mean(tail(indicators$Volume, 20), na.rm = TRUE) * volume_factor)

  # Trend confirmation
  trend_buy <- latest_data$Close > latest_data$SMA200

  # Weighted signal
  total_score <- (
    (macd_buy * 2) +
    (ema20_sma50_buy * 1.5) +
    (ema20_sma20_buy * 1) +
    (sma20_sma50_buy * 1) +
    (rsi_buy * 1.5) +
    (bb_buy * 1) +
    (volume_buy * 1) +
    (trend_buy * 2)
  )

  list(
    MACD_Buy = macd_buy,
    EMA20_SMA50_Buy = ema20_sma50_buy,
    EMA20_SMA20_Buy = ema20_sma20_buy,
    SMA20_SMA50_Buy = sma20_sma50_buy,
    RSI_Buy = rsi_buy,
    BB_Buy = bb_buy,
    Volume_Buy = volume_buy,
    Trend_Buy = trend_buy,
    Total_Score = total_score
  )
}

process_tickers <- function(tickers, period = "all", output_file = "buy_signals.csv", custom_date = Sys.Date(), lookback_period = 5) {
  results <- data.frame(
    Ticker = character(),
    Date = as.Date(character()),
    MACD_Buy = logical(),
    EMA20_SMA50_Buy = logical(),
    EMA20_SMA20_Buy = logical(),
    SMA20_SMA50_Buy = logical(),
    RSI_Buy = logical(),
    BB_Buy = logical(),
    Volume_Buy = logical(),
    Trend_Buy = logical(),
    Total_Score = numeric(),
    stringsAsFactors = FALSE
  )

  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent in :elapsed",
    total = length(tickers), clear = FALSE, width = 60
  )

  for (ticker in tickers) {
    print(ticker)
    pb$tick()  # Update progress bar

    indicators <- screen_stock(ticker, custom_date, period)
    if (is.null(indicators)) {
      print(paste("Skipping ticker", ticker))
      next  # Skip to next iteration if data is insufficient
    }

    # Ensure we have enough data points for the lookback period
    if (nrow(indicators) < lookback_period) {
      print(paste("Not enough data for lookback period for ticker", ticker))
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
      BB_Buy = signals$BB_Buy,
      Volume_Buy = signals$Volume_Buy,
      Trend_Buy = signals$Trend_Buy,
      Total_Score = signals$Total_Score
    )
    print(signal_data)

    results <- rbind(results, signal_data)
  }

  # Add a column that counts the number of TRUE values for each row
  results$Total_True <- rowSums(results[, 3:10])

  # Sort the results based on the Total_Score (descending)
  results <- results[order(-results$Total_Score), ]

  # Write to CSV
  if (file.exists(output_file)) {
    write.table(results, file = output_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    write.table(results, file = output_file, append = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
  }

  return(results)
}

load_tickers <- function(file_path) {
  tickers <- read_csv(file_path)  # Assuming the first column contains tickers
  tickers$Symbol <- gsub("\\^", "-", tickers$Symbol)  # Replace ^ with - for Yahoo Finance
  return(tickers$Symbol)  # Return vector of tickers
}

load_buy_tickers <- function(file_path) {
  tickers <- read_csv(file_path)  # Assuming the first column contains tickers
  return(tickers$Ticker)  # Return vector of tickers
}

current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- paste0("buy_signals_", current_date, ".csv")

#tickers <- load_tickers("/Users/michaelfelix/Documents/GitHub/rtest/tickers.csv")
tickers <- load_buy_tickers("/Users/michaelfelix/Documents/GitHub/rtest/buy_signals_2024-06-19.csv")

#end_date <- as.Date("2024-5-11")

end_date <- Sys.Date()  # today
buy_signals <- process_tickers(tickers, period = "1y", output_file = output_file, custom_date = end_date)
print(buy_signals)
