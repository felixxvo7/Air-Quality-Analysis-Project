# Load necessary libraries
library(tseries)
library(forecast)
library(ggplot2)
library(tidyverse)

# Load dataset
data_path <- "C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv"
data <- read.csv(data_path)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
summary(data)

# Sample 20% for training
set.seed(2025)
train_indices <- sample(1:nrow(data), size = 0.2 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Convert NO2 to time series (training)
no2_series_train <- ts(train_data$NO2.GT., frequency = 24 * 7)
no2_series_test <- ts(test_data$NO2.GT., frequency=24*7)
# Define exogenous variables
exog_vars_train <- as.matrix(train_data[, c("NOx.GT.", "PT08.S5.O3.", "CO.GT.")])
exog_vars_test <- as.matrix(test_data[, c( "NOx.GT.", "PT08.S5.O3.", "CO.GT.")])

# Fit SARIMAX model
sarimax_model <- auto.arima(no2_series_train, xreg = exog_vars_train, seasonal = TRUE)
summary(sarimax_model)

# Forecast for the test set
forecast_values <- forecast(sarimax_model, xreg = exog_vars_test, h = nrow(test_data))

# Optional: Compute RMSE
library(Metrics)
rmse_val <- rmse(test_data$NO2.GT., forecast_values$mean)
cat("Test RMSE:", round(rmse_val, 2), "\n")
#Test RMSE: 28.87


##########################################################################
##VISUALIZATION
##########################################################################

# Prepare dataframe for visualization
forecast_df <- data.frame(
  Time = test_data$Date,
  Actual = no2_series_test,
  Predicted = as.numeric(forecast_values$mean)
)

# Plot actual vs predicted values
ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual NO2"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted NO2"), size = 1, linetype = "dashed") +
  ggtitle("SARIMAX Forecast vs Actual NO2 on Test Set") +
  xlab("Date") + 
  ylab("NO₂ Concentration") +
  scale_color_manual(values = c("Actual NO2" = "red", "Predicted NO2" = "blue")) +
  theme_bw()


checkresiduals(sarimax_model)

###########################################
## For all variable:
###########################################

target_vars <- c( "NO2.GT.","Date","Time" )
exog_vars <- setdiff(colnames(data), target_vars)

# Sample 20% for training
set.seed(2025)
train_indices <- sample(1:nrow(data), size = 0.2 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Convert NO2 to time series (training)
no2_series_train <- ts(train_data$NO2.GT., frequency = 24 * 7)
no2_series_test <- ts(test_data$NO2.GT., frequency=24*7)
# Define exogenous variables
exog_vars_train <- as.matrix(train_data[, exog_vars])
exog_vars_test <- as.matrix(test_data[, exog_vars])


