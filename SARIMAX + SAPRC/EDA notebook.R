# Load libraries
library(tseries)
library(forecast)

data <- read.csv("C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv")

no2_series <- ts(data$NO2.GT., frequency = 24)

plot(no2_series, main="NO2 Time Series", ylab="NO2 Concentration", xlab="Time", col="blue")

adf_test <- adf.test(no2_series)
print(adf_test)


# Compute & Plot ACF (Autocorrelation Function)
acf(no2_series, main="Autocorrelation Function (ACF) for NO2", lag.max=50)

# Compute & Plot PACF (Partial Autocorrelation Function)
pacf(no2_series, main="Partial Autocorrelation Function (PACF) for NO2", lag.max=50)


# Define exogenous variables (CO, NOx, O3, T, RH)
exog_vars <- as.matrix(data[, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH")])

sarimax_model <- auto.arima(no2_series, xreg = exog_vars, seasonal = TRUE)

summary(sarimax_model)

forecast_values <- forecast(sarimax_model, xreg = exog_vars, h = 48)

plot(forecast_values, main="SARIMAX Forecast for NO₂", col="blue")


sarimax_model
