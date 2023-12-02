# Install and Load Necessary Libraries
install.packages(c("quantmod", "rugarch", "forecast", "tseries", "urca"))
library(quantmod)
library(rugarch)
library(forecast)
library(tseries)
library(urca)

# Download and Prepare Data from Yahoo Finance
getSymbols("SPY", src = "yahoo", from = "2010-01-01", to = "2023-01-01")
prices <- Cl(SPY) # Closing prices of SPY
returns <- dailyReturn(prices) # Daily returns

# Check for Stationarity and Unit Roots
adf.test(returns, alternative = "stationary") # Augmented Dickey-Fuller Test

# Examine ACF and PACF of Returns
acf(coredata(returns), main="ACF of Returns") 
pacf(coredata(returns), main="PACF of Returns")

# Fit a Mean Model  (ARIMA)
fit_arima <- auto.arima(returns) #auto.arima iterates through different models to achieve the "best fit" based on AIC/BIC

summary(fit_arima)

Box.test(residuals(fit_arima), type = "Ljung-Box") # Ljung-Box Test for serial correlation

#PACF for order determination of ARCH effects model
residuals_arima <- residuals(fit_arima)

pacf(residuals_arima, main="PACF of ARIMA Model Residuals")
# Check for ARCH Effects using squared residuals
residuals_squared <- residuals_arima^2

lagged_resid_squared <- lag(residuals_squared, k=1)

# Align the lengths by removing NA values
residuals_squared <- residuals_squared[-1]  # Remove the first value which is NA in lagged series
lagged_resid_squared <- lagged_resid_squared[-length(lagged_resid_squared)]  # Remove the last value

lm_model <- lm(residuals_squared ~ lagged_resid_squared)
summary(lm_model)

# Split the Data into Training and Test Sets
split_index <- round(length(returns) * 0.8)
train_returns <- returns[1:split_index]
test_returns <- returns[(split_index+1):length(returns)]

# Fit a Basic GARCH Model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
garch_fit <- ugarchfit(spec = spec, data = train_returns)

# Forecasting and Evaluation
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 10)
forecast_accuracy <- accuracy(garch_forecast, test_returns) # Evaluate forecast accuracy

# Naive Forecast for Comparison
naive_forecast <- mean(train_returns^2)

# Model Diagnostics and Further Analysis
summary(garch_fit) # Model summary
acf(residuals(garch_fit)^2, main="ACF of Squared Residuals") # Check for autocorrelation in residuals

# Experiment with Other Models like EGARCH
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
egarch_fit <- ugarchfit(spec = egarch_spec, data = train_returns)

# Conclude with Analysis
# Compare the models, discuss the findings, and draw conclusions about their effectiveness







