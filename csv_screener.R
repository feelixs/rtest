library(quantmod)
library(TTR)
library(ggplot2)
library(gridExtra)
library(lubridate)

# Function to screen stock and calculate indicators
screen_stock <- function(ticker, period = "all") {
  stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
  
  if (period != "all") {
    end_date <- Sys.Date()
    start_date <- switch(
      period,
      "10m" = end_date - months(10),
      "1y" = end_date - years(1),
      end_date - years(5)
    )
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
}

check_buy_signals <- function(tickers, period = "all", output_file = "buy_signals.csv") {
  results <- data.frame(
    Ticker = character(),
    Date = as.Date(character()),
    MACD_Buy = logical(),
    EMA20_SMA50_Buy = logical(),
    EMA20_SMA20_Buy = logical(),
    SMA20_SMA50_Buy = logical(),
    RSI_Buy = logical(),
    stringsAsFactors = FALSE
  )
  
  for (ticker in tickers) {
    indicators <- screen_stock(ticker, period)
    latest_data <- tail(indicators, 1)
    
    macd_buy <- latest_data$MACD > latest_data$Signal
    ema20_sma50_buy <- latest_data$EMA20 > latest_data$SMA50
    ema20_sma20_buy <- latest_data$EMA20 > latest_data$SMA20
    sma20_sma50_buy <- latest_data$SMA20 > latest_data$SMA50
    rsi_buy <- latest_data$RSI < 30
    
    signal_data <- data.frame(
      Ticker = ticker,
      Date = latest_data$Date,
      MACD_Buy = macd_buy,
      EMA20_SMA50_Buy = ema20_sma50_buy,
      EMA20_SMA20_Buy = ema20_sma20_buy,
      SMA20_SMA50_Buy = sma20_sma50_buy,
      RSI_Buy = rsi_buy
    )
    
    results <- rbind(results, signal_data)
  }
  
  # Add a column that counts the number of TRUE values for each row
  results$Total_True <- rowSums(results[,3:7])
  
  # Sort the results based on the number of TRUE values (descending)
  results <- results[order(-results$Total_True), ]
  
  # Remove the Total_True column before writing to CSV
  results <- results[, -ncol(results)]
  
  # Write to CSV
  if (file.exists(output_file)) {
    write.table(results, file = output_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    write.table(results, file = output_file, append = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
  }
  
  return(results)
}

current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- paste0("buy_signals_", current_date, ".csv")


tickers <- c("AAPL", "GOOGL", "MSFT")
buy_signals <- check_buy_signals(tickers, period = "1y", output_file = output_file)
print(buy_signals)
