# Load libraries
library(tseries)
library(forecast)


data <- read.csv("C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv")
#########################################
## ADF TEST CHECK STATIONARY
#########################################

# Combine Date and Time into Datetime
data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H.%M.%S")

# Sort by datetime
data <- data[order(data$Datetime), ]

# Extract NO2 series
no2_series <- data$NO2.GT.

# Augmented Dickey-Fuller Test
adf_result <- adf.test(no2_series)

# Print result
print(adf_result)
#####################################
## ACF PACF AUTO CORRELATION
#####################################
no2_series <- ts(data$NO2.GT., frequency = 24*7)

plot(no2_series, main="NO2 Time Series", ylab="NO2 Concentration", xlab="Time", col="blue")

adf_test <- adf.test(no2_series)
print(adf_test)


# Compute & Plot ACF (Autocorrelation Function)
acf(no2_series, main="Autocorrelation Function (ACF) for NO2", lag.max=50)

# Compute & Plot PACF (Partial Autocorrelation Function)
pacf(no2_series, main="Partial Autocorrelation Function (PACF) for NO2", lag.max=50)

#######################################
## VISUALIZATION:
#######################################
library(ggplot2)

ggplot(data, aes(x = Datetime, y = NO2.GT.)) +
  geom_line(color = "blue") +
  labs(title = "NO2 Time Series", x = "Date", y = "NO2 Concentration (µg/m³)") +
  theme_minimal()

