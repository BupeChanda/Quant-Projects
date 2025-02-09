
# 1) Load or install required packages

library(quantmod)
library(rugarch)
library(TTR)


# 2) Download Mueller Industries data (NYSE: MLI) from Yahoo for a chosen date range

getSymbols("MLI", 
           src  = "yahoo", 
           from = "2015-12-31", 
           to   = "2024-12-31")


# 3) Calculate Daily Log Returns using the Adjusted Close

mli_returns <- dailyReturn(Ad(MLI), type = "log")


# 4) Fit a GARCH(1,1) model with normal distribution

garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = garch_spec, data = mli_returns)


# 5) Print summary of the GARCH(1,1) model

print(garch_fit)


# 6) Plot the simple returns 

plot(mli_returns,
     main = "MLI Daily Simple Returns (Arithmetic) with 21-day Moving Average",
     ylab = "Simple Returns",
     xlab = "Date")

# Compute a 21-day simple moving average of the returns
mli_SMA <- SMA(mli_returns, n = 50)

# Overlay the 21-day Simple MA in red
lines(mli_SMA, col = "red", lwd = 2)



# 7) Compute mean and standard deviation of log returns
log_return_mean <- mean(mli_returns, na.rm = TRUE)
log_return_sd   <- sd(mli_returns, na.rm = TRUE)


cat("Mean of log returns:", log_return_mean, "\n")
cat("Standard deviation of log returns:", log_return_sd, "\n")
