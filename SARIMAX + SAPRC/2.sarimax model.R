# Load necessary libraries
library(tseries)
library(forecast)
library(ggplot2)
set.seed(2025)
# Load dataset
data_path <- "C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv"
data <- read.csv(data_path)
summary(data)
# Convert Date column (modify format if necessary)
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# **1. Set Seed & Sample 20% of Data for Training**
set.seed(2025)
train_indices <- sample(1:nrow(data), size = 0.2 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]  # Remaining 80% as test set

# **2. Convert NO₂ to Time Series (for Train Data)**
no2_series_train <- ts(train_data$NO2.GT., frequency=24*7)  # Weekly seasonality

# **3. Define Exogenous Variables (for Train Data)**
exog_vars_train <- as.matrix(train_data[, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH")])

# **4. Fit SARIMAX Model Using 20% Sampled Data**
sarimax_model <-auto.arima(train_no2, xreg = train_exog, seasonal = TRUE)
summary(sarimax_model)


# Forecast for the test set length
forecast_values <- forecast(sarimax_model, xreg = test_exog, h = length(test_no2))

forecast_df <- data.frame(
  Time = time(test_no2),
  Actual = as.numeric(test_no2),
  Predicted = as.numeric(forecast_values$mean)
)
# Plot actual vs predicted values
ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual NO₂"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted NO₂"), size = 1, linetype = "dashed") +
  ggtitle("SARIMAX Forecast vs Actual NO₂") +
  xlab("Time") + 
  ylab("NO₂ Concentration") +
  scale_color_manual(values = c("Actual NO₂" = "red", "Predicted NO₂" = "blue")) +
  theme_bw()

# Evaluate model performance
33.66557/mean(test_no2)
ggcorrplot(cor_matrix, method = "square", lab = TRUE, lab_size = 2.5)
