data_path <- "C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv"
data <- read.csv(data_path)

# Load necessary libraries
library(ggplot2)
library(patchwork)

############################################
## Scatter Plots for key relationships
############################################
p1 <- ggplot(data, aes(x = NOx.GT., y = NO2.GT.)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(title = "NOx vs NO2", x = "NOx Concentration", y = "NO2 Concentration")

p2 <- ggplot(data, aes(x = PT08.S5.O3., y = NO2.GT.)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(title = "O3 vs NO2", x = "O3 Concentration", y = "NO2 Concentration")

p3 <- ggplot(data, aes(x = C6H6.GT., y = NO2.GT.)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(title = "C6H6 vs NO2", x = "C6H6 Concentration", y = "NO2 Concentration")

p4 <- ggplot(data, aes(x = CO.GT., y = NO2.GT.)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(title = "CO vs NO2", x = "CO Concentration", y = "NO2 Concentration")

# Combine into one layout (2x2 grid)
(p1 | p2) / (p3 | p4)
 
#############################################################33
#Seasonality Analysis for NO₂

library(forecast)

# Convert NO2 to time series (assuming hourly data)
no2_ts <- ts(data$NO2.GT., frequency = 24 * 7)  # 24 hours * 7 days = weekly pattern

# Apply STL decomposition
decomp <- stl(no2_ts, s.window = "periodic")

# Plot the decomposition
plot(decomp, main = "STL Decomposition of NO2 Time Series")
# Decompose time series (STL)

autoplot(decomp) + ggtitle("Seasonal Decomposition of NO2 Time Series")


# You’ll get four panels:
# Original series
# Seasonal component (cyclical patterns like time-of-day or weekly)
# Trend (long-term movement)
# Remainder (random noise)

# BY

# Convert NO2 to time series (assuming hourly data)
no2_ts_month <- ts(data$NO2.GT., frequency = 24 * 30)  # 24 hours * 7 days = weekly pattern

# Apply STL decomposition
decomp_month <- stl(no2_ts_month, s.window = "periodic")

# Plot the decomposition
plot(decomp_month, main = "STL Decomposition of NO2 Time Series by Month")

ggcorrplot(cor_matrix, method = "square", lab = TRUE, lab_size = 2.5)
