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

    # Calculate stop loss (2 ATR below close)
    stop_loss <- Cl(stock_data) - (2 * atr[,"atr"])

    # Calculate implied volatility (using historical volatility as a proxy)
    implied_volatility <- volatility(Cl(stock_data), n = 30, calc = "close")

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
      ATR = as.numeric(atr[,"atr"]),
      ROC = as.numeric(roc),
      BB_Upper = as.numeric(bb[,"up"]),
      BB_Lower = as.numeric(bb[,"dn"]),
      BB_Middle = as.numeric(bb[,"mavg"]),
      Stop_Loss = as.numeric(stop_loss),
      Implied_Volatility = as.numeric(implied_volatility)
    )
    return(indicators)

  }, error = function(e) {
    print(paste("Error for ticker", ticker, ":", conditionMessage(e)))
    return(NULL)
  })
}

chart_stock <- function(indicators, ticker = "Stock") {
  # Plot closing prices with SMA, EMA, Bollinger Bands, and Stop Loss
  p1 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close")) +
    geom_line(aes(y = SMA20, color = "SMA20")) +
    geom_line(aes(y = SMA50, color = "SMA50")) +
    geom_line(aes(y = SMA200, color = "SMA200")) +
    geom_line(aes(y = EMA20, color = "EMA20"), linetype = "dashed") +
    geom_line(aes(y = EMA50, color = "EMA50"), linetype = "dashed") +
    geom_line(aes(y = EMA200, color = "EMA200"), linetype = "dashed") +
    geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "lightblue", alpha = 0.2) +
    geom_line(aes(y = BB_Middle, color = "BB Middle")) +
    geom_line(aes(y = Stop_Loss, color = "Stop Loss"), linetype = "dotted", size = 1) +
    labs(title = paste(ticker, "Chart"),
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

  # Plot Volume
  p4 <- ggplot(indicators, aes(x = Date)) +
    geom_col(aes(y = Volume), fill = "darkblue", alpha = 0.5) +
    labs(title = "Volume",
         y = "Volume") +
    theme_minimal()

  # Plot Implied Volatility
  p5 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = Implied_Volatility, color = "Implied Volatility")) +
    labs(title = "Implied Volatility",
         y = "Volatility",
         color = "Legend") +
    theme_minimal()

  # Plot ROC
  p6 <- ggplot(indicators, aes(x = Date)) +
    geom_line(aes(y = ROC, color = "ROC")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Rate of Change (ROC)",
         y = "ROC",
         color = "Legend") +
    theme_minimal()

  # Combine plots
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
}

check_buy_signals <- function(indicators, lookback_days = 5, rsi_threshold = 30, volume_increase = 1.5) {
  latest_data <- tail(indicators, 1)
  lookback_data <- tail(indicators, lookback_days + 1)

  crossover_occurred <- function(short, long) {
    for (i in 1:lookback_days) {
      if (lookback_data[i, short] <= lookback_data[i, long] && lookback_data[i + 1, short] > lookback_data[i + 1, long]) {
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
    if (lookback_data[i, "RSI"] < rsi_threshold && lookback_data[i + 1, "RSI"] >= rsi_threshold) {
      rsi_buy <- TRUE
      break
    }
  }

  volume_confirmed <- latest_data$Volume > (mean(lookback_data$Volume) * volume_increase)
  trend_bullish <- latest_data$Close > latest_data$SMA200
  momentum_bullish <- latest_data$ROC > 0
  volatility_low <- latest_data$ATR < mean(lookback_data$ATR)
  bb_squeeze <- (latest_data$BB_Upper - latest_data$BB_Lower) < mean(lookback_data$BB_Upper - lookback_data$BB_Lower)

  score <- sum(c(
    macd_buy * 2,
    ema20_sma50_buy * 1.5,
    ema20_sma20_buy * 1,
    sma20_sma50_buy * 1,
    rsi_buy * 1,
    volume_confirmed * 1,
    trend_bullish * 2,
    momentum_bullish * 1,
    volatility_low * 0.5,
    bb_squeeze * 1
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
    Buy_Score = score,
    Stop_Loss = latest_data$Stop_Loss
  )
}

ticker <- "ABCB"
end_date <- Sys.Date()

indicators <- screen_stock(ticker, period = "1y", custom_date = end_date)
chart_stock(indicators, ticker)
buy_signals <- check_buy_signals(indicators)
print(buy_signals)