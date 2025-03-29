# Load libraries
library(tseries)
library(forecast)
library(ggplot2)
library(dplyr)
library(Metrics)  # for RMSE

# Load dataset
data_path <- "C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/SARIMAX + SAPRC/SAPRC_data.csv"
data <- read.csv(data_path)

# === Split into training and test sets ===
set.seed(2025)
train_indices <- sample(1:nrow(data), size = 0.2 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Convert NO2 to time series
no2_train <- ts(train_data$NO2.GT., frequency = 24 * 7)
no2_test <- ts(test_data$NO2.GT., frequency = 24 * 7)

# === 1. SARIMAX Model with Proxy Variables ===
xreg_proxy_train <- as.matrix(train_data[, c("PAN_proxy", "HONO_proxy", "SAPRC_proxy")])
xreg_proxy_test <- as.matrix(test_data[, c("PAN_proxy", "HONO_proxy", "SAPRC_proxy")])

sarimax_proxy <- auto.arima(no2_train, xreg = xreg_proxy_train, seasonal = TRUE)
forecast_proxy <- forecast(sarimax_proxy, xreg = xreg_proxy_test, h = nrow(test_data))
rmse_proxy <- rmse(test_data$NO2.GT., forecast_proxy$mean)

# === 2. SARIMAX Model with Full Exogenous Variables ===
xreg_full_train <- as.matrix(train_data[, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH",
                                            "PAN_proxy", "HONO_proxy", "SAPRC_proxy")])
xreg_full_test <- as.matrix(test_data[, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH",
                                          "PAN_proxy", "HONO_proxy", "SAPRC_proxy")])

sarimax_full <- auto.arima(no2_train, xreg = xreg_full_train, seasonal = TRUE)
forecast_full <- forecast(sarimax_full, xreg = xreg_full_test, h = nrow(test_data))
rmse_full <- rmse(test_data$NO2.GT., forecast_full$mean)

# === Print Results ===
cat("SARIMAX with Proxy Variables - RMSE:", round(rmse_proxy, 2), "\n")
#RMSE: 24.07
cat("SARIMAX with Full Variables - RMSE:", round(rmse_full, 2), "\n")
#RMSE: 21.73 


##########################################################################
##VISUALIZATION
##########################################################################

# Prepare forecast dataframe for proxy model
forecast_proxy_df <- data.frame(
  Time = test_data$Date,
  Actual = test_data$NO2.GT.,
  Predicted = as.numeric(forecast_proxy$mean)
)

# Plot: Proxy model
ggplot(forecast_proxy_df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual NO2"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted NO2 (Proxy)"), size = 1, linetype = "dashed") +
  ggtitle("SARIMAX Forecast with Proxy Variables vs Actual NO2") +
  xlab("Date") +
  ylab("NO2 Concentration") +
  scale_color_manual(values = c("Actual NO2" = "red", "Predicted NO2 (Proxy)" = "blue")) +
  theme_bw()
 
# Prepare forecast dataframe for full model
forecast_full_df <- data.frame(
  Time = test_data$Date,
  Actual = test_data$NO2.GT.,
  Predicted = as.numeric(forecast_full$mean)
)

# Plot: Full model
ggplot(forecast_full_df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual NO2"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted NO2 (Full)"), size = 1, linetype = "dashed") +
  ggtitle("SARIMAX Forecast with Full Variables vs Actual NO2") +
  xlab("Date") +
  ylab("NO2 Concentration") +
  scale_color_manual(values = c("Actual NO2" = "red", "Predicted NO2 (Full)" = "darkgreen")) +
  theme_bw()

combined_df$Residual_Proxy <- combined_df$Actual - combined_df$Predicted_Proxy
combined_df$Residual_Full <- combined_df$Actual - combined_df$Predicted_Full

# Residual distribution
ggplot() +
  geom_density(aes(x = combined_df$Residual_Proxy, color = "Proxy Residuals"), size = 1) +
  geom_density(aes(x = combined_df$Residual_Full, color = "Full Residuals"), size = 1) +
  ggtitle("Residual Distribution: Proxy vs Full Model") +
  xlab("Prediction Error (Actual - Predicted)") +
  ylab("Density") +
  scale_color_manual(values = c("Proxy Residuals" = "blue", "Full Residuals" = "darkgreen")) +
  theme_bw()

####################
#
#####################
checkresiduals(sarimax_full)

combined_df <- data.frame(
  Time = test_data$Date,
  Actual = test_data$NO2.GT.,
  Predicted = as.numeric(forecast_full$mean)
)

ggplot(combined_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Scatter Plot: Predicted vs Actual NO2") +
  xlab("Actual NO2") + ylab("Predicted NO2") +
  theme_minimal()


combined_df$Residual <- combined_df$Actual - combined_df$Predicted

ggplot(combined_df, aes(x = Actual, y = Residual)) +
  geom_point(alpha = 0.5, color = "tomato") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  ggtitle("Prediction Error (Residuals) vs Actual NO₂") +
  xlab("Actual NO₂") + ylab("Residual (Actual - Predicted)") +
  theme_minimal()
