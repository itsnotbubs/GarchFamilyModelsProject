# Install and Load Necessary Libraries
install.packages(c("quantmod", "rugarch", "forecast", "tseries", "urca", "FinTS", "ggplot2"))
library(quantmod)
library(rugarch)
library(forecast)
library(tseries)
library(urca)
library(FinTS)
library(ggplot2)

# Download and Prepare Data from Yahoo Finance
getSymbols("SPY", src = "yahoo", from = "2010-01-01", to = "2023-01-01")
prices <- Cl(SPY) # Closing prices of SPY
returns <- dailyReturn(prices) 
mean_volatility <- sd(returns)
print(mean_volatility)*100

# Histogram of Daily Returns
ggplot(data = data.frame(returns = coredata(returns)), aes(x = returns)) +
  geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Daily Returns for SPY", x = "Daily Returns", y = "Frequency")

# Check for Stationarity
adf_test_results <- adf.test(returns, alternative = "stationary") # Augmented Dickey-Fuller Test
print(adf_test_results)
# Fit a Mean Model  (ARIMA)
fit_arima <- auto.arima(returns) #auto.arima iterates through different models to achieve the "best fit" based on AIC/BIC

# Ljung-Box Test for serial correlation
box_test_results <- Box.test(residuals(fit_arima), type = "Ljung-Box")

print(box_test_results)
# Test to see if ARCH effects are present
arch_test_results <- ArchTest(returns)
print(arch_test_results)

garch_mod <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = auto.arima(returns)$arma, include.mean = TRUE))

garch_fit <- ugarchfit(garch_mod, data = returns)

garch_resid <- residuals(garch_fit, standardize=TRUE)

print(garch_fit)

# Extract conditional volatility
garch_volatility <- sigma(garch_fit)

# Create a time series object
garch_vol_ts <- ts(garch_volatility, start = c(2010), frequency = 365)

# Plot the GARCH volatility
plot(garch_vol_ts, xlab = "Year", ylab = "Volatility", main = "GARCH Volatility [1,1]")


#egarch portion
egarch_mod <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = auto.arima(returns)$arma, include.mean = TRUE))

egarch_fit <- ugarchfit(egarch_mod, data =returns)

print(egarch_fit)

# Extract conditional volatility
egarch_volatility <- sigma(egarch_fit)

# Create a time series object
egarch_vol_ts <- ts(egarch_volatility, start = c(2010), frequency = 365)

# Plot the EGARCH volatility
plot(egarch_vol_ts, xlab = "Year", ylab = "Volatility", main = "EGARCH Volatility [1,1]")


tgarch_mod <- ugarchspec(variance.model = list(model = "fGARCH",
                          submodel = "TGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = auto.arima(returns)$arma, 
                                            include.mean = TRUE))
tgarch_fit <- ugarchfit(tgarch_mod, data =returns)

print(tgarch_fit)

# Extract conditional volatility
tgarch_volatility <- sigma(tgarch_fit)

# Create a time series object
tgarch_vol_ts <- ts(tgarch_volatility, start = c(2010), frequency = 365)

# Plot the EGARCH volatility
plot(tgarch_vol_ts, xlab = "Year", ylab = "Volatility", main = "tGARCH Volatility [1,1]")
