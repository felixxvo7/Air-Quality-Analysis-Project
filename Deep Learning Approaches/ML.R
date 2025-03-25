setwd("C:/Users/DucDo/OneDrive-UniversityofManitoba/Documents/University_documents/UofM/WInter2025/DATA2010/Air-Quality-Analysis-Project")

df = read.csv("final_cleaned_data.csv")
head(df)

sum(df == -200)
colSums(df == -200)

typeof(df$Date)
typeof(df$Time)
typeof(df)

# df2 <- read.csv("formated_data.csv")
# head(df2)

# 1. Calculate correlation matrix (excluding columns 1 & 2)
cor_matrix <- cor(as.matrix(df[-c(1, 2)]))

# 3. Better alternative using ggplot2
library(ggplot2)
library(reshape2) # For melt() function

correlation_heatmap = function(cor_matrix) {
  # Convert correlation matrix to long format
  melted_cor <- melt(cor_matrix)
  
  # Create heatmap
  ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
}

correlation_heatmap(cor_matrix = cor_matrix)

library(tidyverse)

# Define predictors (exclude Date, Time, and targets)
predictors <- df %>% select(-c(Date, Time, CO.GT., C6H6.GT., NOx.GT., NO2.GT.))

# Define multiple target variables (CO, NOx, Benzene)
targets <- df %>% select(CO.GT., C6H6.GT., NOx.GT., NO2.GT.)

# Normalize data (optional but recommended)

normalize <- function(x) { 
  (x - min(x)) / (max(x) - min(x)) 
}

predictors <- as.data.frame(lapply(predictors, normalize))
# targets <- as.data.frame(lapply(targets, normalize))

# Convert to matrices for Keras
X <- as.matrix(predictors)
# Y <- as.matrix(targets)

correlation_heatmap(cor(X))

# we can see that T, RH and AH are not very correlated to sensor data and pollutant concentration, so we should exclude them.

predictors <- predictors %>% select(-c("T", "RH", "AH"))
################################## KNN

library(FNN)

# Split into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_x <- predictors[trainIndex, ]
train_y <- targets[trainIndex, ]
test_x <- predictors[-trainIndex, ]
test_y <- targets[-trainIndex, ]

knn_result <- function(target) {
  # Train kNN regression model
  knn_fit <- knn.reg(train = train_x,
                        test = test_x,
                        y = train_y[[target]],
                        k = 5)  # k = 5 nearest neighbors
  
  # Predictions
  predictions <- knn_fit$pred
  
  # Evaluate model performance for multiple targets
  mse <- mean((predictions - test_y[[target]])^2)  # Compute MSE for each target
  # return (mse)
  
  # Normalize MSE by the mean of the actual values
  target_mean <- mean(test_y[[target]])
  mse_normalized <- mse / target_mean
  
  return(mse_normalized)  # Return the normalized MSE for this target
}


# CO.GT.     C6H6.GT.      NOx.GT.      NO2.GT. 
# 3.430058e-01 1.009973e+02 9.607039e+04 1.366777e+04 

### Well that is bad. Hold on the code is fucked.

# Example usage for one target variable:
mse_CO <- knn_result("CO.GT.") # 0.3430058        # 0.1651042
mse_C6H6 <- knn_result("C6H6.GT.") # 15.61902     # 1.606616
mse_NOx <- knn_result("NOx.GT.") # 5837.382       # 24.51751
mse_NO2 <- knn_result("NO2.GT.") # 453.9531       # 4.138624

# Display MSE for each target
mse_CO
mse_C6H6
mse_NOx
mse_NO2

### Still pretty bad. The normalized mse for NOx is 2400% of the mean.

############################ use caret library
library(caret)

train = cbind(train_x, train_y)

# List of target variables
target_names <- c("CO.GT.", "C6H6.GT.", "NOx.GT.", "NO2.GT.")

# Initialize an empty list to store the predictions
predictions_caret_list <- list()

# Loop through each target and train a model
for (target in target_names) {
  # Train kNN model for each target variable
  knn_fit <- train(
    as.formula(paste(target, "~ .")), 
    data = train, 
    method = "knn", 
    tuneGrid = expand.grid(k = 5),  # Specify number of neighbors
    trControl = trainControl(method = "cv", number = 10)  # Cross-validation
  )
  
  # Make predictions
  predictions_caret_list[[target]] <- predict(knn_fit, newdata = test_x)
}

# Check the predictions for each target
predictions_caret_list
