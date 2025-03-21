# Load libraries
library(tseries)
library(forecast)

data <- read.csv("C:/Users/felix/Desktop/CODING/felix's works/Air-Quality-Analysis-Project/data/final_cleaned_data.csv")

no2_series <- ts(data$NO2.GT., frequency = 24)

plot(no2_series, main="NO2 Time Series", ylab="NO2 Concentration", xlab="Time", col="blue")

adf_test <- adf.test(no2_series)
print(adf_test)
