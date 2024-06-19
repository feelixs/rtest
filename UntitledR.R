library(quantmod)
library(TTR)
library(ggplot2)
library(gridExtra)
library(lubridate)

screen_stock <- function(ticker, period = "all") {
  # Get stock data
  stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
  
  # Subset data based on the period
  if (period != "all") {
    end_date <- Sys.Date()
    start_date <- switch(
      period,
      "10m" = end_date - months(10),
      "1y" = end_date - years(1),
      end_date - years(5) # default to 5 years if period not recognized
    )
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    stock_data <- stock_data[paste(start_date, end_date, sep = "::")]
  }
  
  # Print the date range of the subset data for debugging
  print(paste("Data Range:", index(stock_data)[1], "to", index(stock_data)[nrow(stock_data)]))
  
  # Calculate RSI
  rsi <- RSI(Cl(stock_data), n = 14)
  
  # Calculate MACD
  macd <- MACD(Cl(stock_data), nFast = 12, nSlow = 26, nSig = 9)
  
  # Calculate SMA
  sma20 <- SMA(Cl(stock_data), n = 20)
  sma50 <- SMA(Cl(stock_data), n = 50)
  sma200 <- SMA(Cl(stock_data), n = 200)
  
  # Calculate EMA
  ema20 <- EMA(Cl(stock_data), n = 20)
  ema50 <- EMA(Cl(stock_data), n = 50)
  ema200 <- EMA(Cl(stock_data), n = 200)
  
  # Combine indicators into a data frame
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
  
  # Remove rows with NA values
  indicators <- na.omit(indicators)
  
  return(indicators)
}

chart_stock <- function(indicators) {
  # Plot closing prices with SMA and EMA
  p1 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close")) +
    geom_line(aes(y = SMA20, color = "SMA20")) +
    geom_line(aes(y = SMA50, color = "SMA50")) +
    geom_line(aes(y = SMA200, color = "SMA200")) +
    geom_line(aes(y = EMA20, color = "EMA20"), linetype = "dashed") +
    geom_line(aes(y = EMA50, color = "EMA50"), linetype = "dashed") +
    geom_line(aes(y = EMA200, color = "EMA200"), linetype = "dashed") +
    labs(title = "Stock Price with SMA and EMA",
         y = "Price",
         color = "Legend") +
    theme_minimal()
  
  # Plot RSI
  p2 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = RSI, color = "RSI")) +
    labs(title = "Relative Strength Index (RSI)",
         y = "RSI",
         color = "Legend") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "blue") +
    theme_minimal()
  
  # Plot MACD
  p3 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = MACD, color = "MACD")) +
    geom_line(aes(y = Signal, color = "Signal")) +
    labs(title = "MACD",
         y = "Value",
         color = "Legend") +
    theme_minimal()
  
  # Combine plots
  grid.arrange(p1, p2, p3, ncol = 1)
}

# Example usage:
indicators <- screen_stock("AAPL", period = "all")
chart_stock(indicators)