library(quantmod)
library(TTR)
library(ggplot2)
library(gridExtra)
library(lubridate)

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

check_buy_signals <- function(indicators, lookback_days = 5) {
  latest_data <- tail(indicators, 1)
  lookback_data <- tail(indicators, lookback_days + 1)

  # Helper function to check if a crossover occurred within the last 'lookback_days'
  crossover_occurred <- function(short, long) {
    for (i in 1:lookback_days) {
      if (lookback_data[i, short] <= lookback_data[i, long] && lookback_data[i + 1, short] > lookback_data[i + 1, long]) {
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

  # RSI buy signal: RSI dropped below 30 and then risen above it within the lookback period
  rsi_buy <- FALSE
  for (i in 1:lookback_days) {
    if (lookback_data[i, "RSI"] < 30 && lookback_data[i + 1, "RSI"] >= 30) {
      rsi_buy <- TRUE
      break
    }
  }

  list(
    MACD_Buy = macd_buy,
    EMA20_SMA50_Buy = ema20_sma50_buy,
    EMA20_SMA20_Buy = ema20_sma20_buy,
    SMA20_SMA50_Buy = sma20_sma50_buy,
    RSI_Buy = rsi_buy
  )
}

chart_stock <- function(indicators, ticker = "Stock") {
  # Plot closing prices with SMA and EMA
  p1 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close")) +
    geom_line(aes(y = SMA20, color = "SMA20")) +
    geom_line(aes(y = SMA50, color = "SMA50")) +
    geom_line(aes(y = SMA200, color = "SMA200")) +
    geom_line(aes(y = EMA20, color = "EMA20"), linetype = "dashed") +
    geom_line(aes(y = EMA50, color = "EMA50"), linetype = "dashed") +
    geom_line(aes(y = EMA200, color = "EMA200"), linetype = "dashed") +
    labs(title = paste(ticker, " Chart"),
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

ticker <- "aan"

end_date <- as.Date("2024-5-11")
indicators <- screen_stock(ticker, period = "1y", custom_date = end_date)
chart_stock(indicators, ticker)
buy_signals <- check_buy_signals(indicators)
print(buy_signals)

