
AirQualityUCI.csv <- read_csv("Documents/GitHub/Air-Quality-Analysis-Project/Datasets/formated_data.csv")
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
minus200_counts <- colSums(filtered_data[, -c(1,2)] == -200, na.rm = TRUE)
print(rows_with_minus200)
print(minus200_counts)

# Frequency table of counts -200 by rows
filtered_data %>%
  mutate(
    minus200_count = rowSums(across(where(is.numeric)) == -200, na.rm = TRUE)
  ) %>%
  group_by(minus200_count) %>%
  tally()  

###############__________________________________________________________________________________________________________#########
AirQualityUCI.csv <- read_csv("Documents/GitHub/Air-Quality-Analysis-Project/Datasets/final_cleaned_data.csv")


# Select numeric columns only
numeric_data <- filtered_data %>% select(-Date) %>% select(-Time) 
numeric_data <- numeric_data %>%
                  mutate(across(where(is.numeric), ~ ifelse(.x == -200, NA, .x))) # Final safety check

# Generate summary statistics
summary_stats <- data.frame(
  Variable = names(numeric_data),
  Mean = sapply(numeric_data, mean, na.rm = TRUE),
  Median = sapply(numeric_data, median, na.rm = TRUE),
  SD = sapply(numeric_data, sd, na.rm = TRUE),
  Min = sapply(numeric_data, min, na.rm = TRUE),
  Max = sapply(numeric_data, max, na.rm = TRUE),
  Q1 = sapply(numeric_data, quantile, probs = 0.25, na.rm = TRUE),
  Q3 = sapply(numeric_data, quantile, probs = 0.75, na.rm = TRUE),
  IQR = sapply(numeric_data, IQR, na.rm = TRUE),
  NAs = sapply(numeric_data, function(x) sum(is.na(x)))
)

# Print formatted summary
print("Summary Statistics of Air Quality Dataset:")
print(summary_stats, row.names = FALSE)

# Generate distribution plots
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
for (col in names(numeric_data)) {
  hist(numeric_data[[col]], main = paste("Histogram of", col), 
       xlab = col, col = "skyblue", breaks = 30)
  boxplot(numeric_data[[col]], main = paste("Boxplot of", col), 
          col = "lightgreen", horizontal = TRUE)
}

df[df == -200] <- 0
summary(df[,-c(1,2)])


