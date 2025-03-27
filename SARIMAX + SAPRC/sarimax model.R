# Load necessary libraries
library(tseries)
library(forecast)
library(ggplot2)
set.seed(2025)
# Load dataset
data_path <- "C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv"
data <- read.csv(data_path)

# Convert NO2 column to time series (assuming hourly data)
no2_series <- ts(data$NO2.GT., frequency = 24)

# Define exogenous variables (CO, NOx, O3, T, RH)
exog_vars <- as.matrix(data[, c("CO.GT.", "NOx.GT.", "PT08.S5.O3.", "T", "RH")])

# Split into training (80%) and test (20%) sets
train_size <- floor(0.8 * length(no2_series))  # 80% for training
train_no2 <- no2_series[1:train_size]
test_no2 <- no2_series[(train_size + 1):length(no2_series)]

train_exog <- exog_vars[1:train_size, ]
test_exog <- exog_vars[(train_size + 1):nrow(exog_vars), ]

# Check for missing values
if (any(is.na(train_no2)) || any(is.na(train_exog))) {
  stop("Training data contains missing values. Please handle them before fitting the model.")
}

# Fit SARIMAX model on the training set
sarimax_model <- auto.arima(train_no2, xreg = train_exog, seasonal = TRUE)

# Display model summary
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
