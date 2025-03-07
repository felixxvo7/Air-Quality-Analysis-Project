
AirQualityUCI.csv <- read.csv("C:/Users/felix/Desktop/AirQualityUCI.csv", header=TRUE, sep=";")
sum(is.na(AirQualityUCI.csv))

sum(is.na(AirQualityUCI.csv[9470,]))
# Load dplyr
library(dplyr)
library(tidyverse)
library(readr)

# 1. Clean full NA rows and columns
filtered_data <- AirQualityUCI.csv %>%
  select(where(~ sum(is.na(.)) != 9471))%>%
  filter(rowSums(is.na(AirQualityUCI.csv)) != 10)

View(filtered_data)

# 2. Convert columns char to numeric -> "," to "." 
filtered_data[3:ncol(filtered_data)] <- lapply(
  filtered_data[3:ncol(filtered_data)], 
  function(x) as.numeric(gsub(",", ".", x))
)
View(filtered_data)

# Find columns with at least one value between -200 and 0
# filtered_data[filtered_data > -200 & filtered_data < 0]
cols_with_negatives <- names(filtered_data)[sapply(filtered_data, function(x) {
  any(x > -200 & x < 0, na.rm = TRUE)
})]
# Print the column  with negative beside -200
print(cols_with_negatives)

# 3. Convert "dd/mm/yyyy" to Date type
filtered_data$Date <- as.Date(filtered_data$Date, format = "%d/%m/%Y")

str(filtered_data)


# 4. Filter rows containing -200 in any column
rows_with_minus200 <- filtered_data[rowSums(filtered_data == -200, na.rm = TRUE) > 0, ]
minus200_counts <- colSums(filtered_data == -200, na.rm = TRUE)
print(rows_with_minus200)
print(minus200_counts)

# Frequency table of counts -200 by rows
filtered_data %>%
  mutate(
    minus200_count = rowSums(across(where(is.numeric)) == -200, na.rm = TRUE)
  ) %>%
  group_by(minus200_count) %>%
  tally()  


