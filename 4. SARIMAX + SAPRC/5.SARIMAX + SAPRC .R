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


# Load data
future_data <- read.csv("C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/SARIMAX + SAPRC/SAPRC_data.csv")

# Ensure future_data has enough rows (at least 48 for 48-hour forecast)
future_xreg <- as.matrix(future_data[1:48, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH",
                                             "PAN_proxy", "HONO_proxy", "SAPRC_proxy")])

# Forecast next 48 hours
future_forecast <- forecast(sarimax_full, xreg = future_xreg, h = 48)

# Create timestamps for future prediction (assuming hourly data)
last_time <- as.POSIXct(tail(data$Date, 1))
future_time <- seq(from = last_time + 3600, by = "hour", length.out = 48)

# Build forecast dataframe
forecast_df <- data.frame(
  Time = future_time,
  Forecast = as.numeric(future_forecast$mean),
  Lower = future_forecast$lower[, 2],
  Upper = future_forecast$upper[, 2]
)

# Plot
library(ggplot2)
ggplot(forecast_df, aes(x = Time, y = Forecast)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightgreen", alpha = 0.3) +
  ggtitle("48-Hour Forecast of NO₂ (SARIMAX Full)") +
  xlab("Time") + ylab("Predicted NO₂") +
  theme_minimal()

data$Date <- as.Date(data$Date)
# === Load libraries ===
library(forecast)
library(dplyr)
library(lubridate)
library(ggplot2)

# === Load dataset ===
data <- read.csv("SAPRC_data.csv")
data$Date <- as.Date(data$Date)

# === Aggregate to weekly averages ===
weekly_data <- data %>%
  mutate(Week = floor_date(Date, unit = "week")) %>%
  group_by(Week) %>%
  summarise(
    NO2 = mean(NO2.GT., na.rm = TRUE),
    CO = mean(CO.GT., na.rm = TRUE),
    NOx = mean(NOx.GT., na.rm = TRUE),
    O3 = mean(PT08.S5.O3., na.rm = TRUE),
    T = mean(T, na.rm = TRUE),
    RH = mean(RH, na.rm = TRUE),
    PAN = mean(PAN_proxy, na.rm = TRUE),
    HONO = mean(HONO_proxy, na.rm = TRUE),
    SAPRC = mean(SAPRC_proxy, na.rm = TRUE)
  ) %>%
  ungroup()

# === Extract Week of Year for seasonal pattern ===
weekly_data$WeekOfYear <- isoweek(weekly_data$Week)

# === Build time series and xreg matrix ===
ts_no2 <- ts(weekly_data$NO2, frequency = 52)
xreg_weekly <- as.matrix(weekly_data[, c("CO", "NOx", "O3", "T", "RH", "PAN", "HONO", "SAPRC")])

# === Fit SARIMAX model ===
model_weekly <- auto.arima(ts_no2, xreg = xreg_weekly, seasonal = TRUE)

# === Build seasonal xreg for future (12 weeks) ===
seasonal_means <- weekly_data %>%
  group_by(WeekOfYear) %>%
  summarise(across(c(CO, NOx, O3, T, RH, PAN, HONO, SAPRC), mean, na.rm = TRUE)) %>%
  ungroup()

start_week <- (isoweek(max(weekly_data$Week)) + 1) %% 52
week_indices <- ((start_week - 1 + 0:11) %% 52) + 1
future_xreg <- as.matrix(seasonal_means[match(week_indices, seasonal_means$WeekOfYear), -1])

# === Forecast next 12 weeks ===
forecast_result <- forecast(model_weekly, xreg = future_xreg, h = 12)

# === Generate future dates ===
last_week <- max(weekly_data$Week)
future_weeks <- seq(from = last_week + 7, by = "week", length.out = 12)

forecast_df <- data.frame(
  Week = future_weeks,
  Forecast = as.numeric(forecast_result$mean),
  Lower = forecast_result$lower[, 1],
  Upper = forecast_result$upper[, 1]
)

# === Combine historical and forecasted data ===
full_plot_df <- weekly_data %>%
  select(Week, NO2) %>%
  rename(Value = NO2) %>%
  mutate(Type = "Actual") %>%
  bind_rows(
    forecast_df %>%
      select(Week, Forecast) %>%
      rename(Value = Forecast) %>%
      mutate(Type = "Forecast")
  )

# === Final plot ===
ggplot() +
  geom_line(data = full_plot_df, aes(x = Week, y = Value, color = Type), size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Week, ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
  geom_vline(xintercept = as.numeric(max(weekly_data$Week)), linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "steelblue", "Forecast" = "darkgreen")) +
  ggtitle("12-Week NO₂ Forecast (SARIMAX Full Model with Seasonal Inputs)") +
  xlab("Week") + ylab("Average NO₂ (µg/m³)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal()

# === Merge actual and forecast as one continuous line ===
continuous_df <- weekly_data %>%
  select(Week, NO2) %>%
  rename(Value = NO2) %>%
  bind_rows(
    forecast_df %>%
      select(Week, Forecast) %>%
      rename(Value = Forecast)
  )

# === Plot with continuous line ===
ggplot() +
  geom_line(data = continuous_df, aes(x = Week, y = Value), color = "darkgreen", size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Week, ymin = Lower, ymax = Upper), 
              fill = "gray", alpha = 0.3) +
  geom_vline(xintercept = as.numeric(max(weekly_data$Week)), linetype = "dashed") +
  ggtitle("12-Week NO2 Forecast (SARIMAX Full Model with SAPRC)") +
  xlab("Week") + ylab("Average NO₂ (µg/m³)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal()
