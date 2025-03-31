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
set.seed(2025)
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
  
  rmse <- sqrt(mse)
  
  # return (rmse)
  
  # Normalize MSE by the mean of the actual values
  target_mean <- mean(test_y[[target]])
  mse_normalized <- rmse / target_mean
  
  return(mse_normalized)  # Return the normalized MSE for this target
}


# CO.GT.     C6H6.GT.      NOx.GT.      NO2.GT. 
# 3.430058e-01 1.009973e+02 9.607039e+04 1.366777e+04 

### Well that is bad. Hold on the code is fucked.

# Example usage for one target variable:
rmse_CO <- knn_result("CO.GT.") # 0.3430058        # 0.5778774   # 0.2807137
rmse_C6H6 <- knn_result("C6H6.GT.") # 15.61902     # 3.601207    # 0.3714302
rmse_NOx <- knn_result("NOx.GT.") # 5837.382       # 75.71713    # 0.3230885
rmse_NO2 <- knn_result("NO2.GT.") # 453.9531       # 21.05513    # 0.1953094

# Display MSE for each target
rmse_CO
rmse_C6H6
rmse_NOx
rmse_NO2

### NO2 is the best, C6H6 is the worst.

############################ use caret library

# library(caret)
# 
train = cbind(train_x, train_y)
# 
# # List of target variables
# # target_names <- c("CO.GT.", "C6H6.GT.", "NOx.GT.", "NO2.GT.")
# 
# #######################################################
# 
# # Train KNN model using caret
# 
# knn_fit <- train(NO2.GT. ~ PT08.S1.CO. + PT08.S2.NMHC. + PT08.S3.NOx. + PT08.S4.NO2. + PT08.S5.O3., 
#                  data = train, 
#                  method = "knn",
#                  tuneGrid = expand.grid(k = 5),  # Specify number of neighbors
#                  trControl = trainControl(method = "cv", number = 10))  # Cross-validation
# 
# # Predictions
# predictions_caret <- predict(knn_fit, newdata = test_x)
# 
# # Evaluate performance
# mse_caret <- mean((predictions_caret - test_y$CO.GT.)^2)
# mse_caret


# 0.3503605 84.74771 91115.17 13220.71

#### this is useless

######################################################## Random forest

library(randomForest)

# test <- cbind(test_x, test_y)

rf_result <- function(target) {
  rf_model <- randomForest(as.formula(paste(target, "~ PT08.S1.CO. + PT08.S2.NMHC. + PT08.S3.NOx. + PT08.S4.NO2. + PT08.S5.O3.")),  
                           data = train)
                           #importance = TRUE)  
  
  predictions <- predict(rf_model, test_x)
  
  # Evaluate model performance for multiple targets
  mse <- mean((predictions - test_y[[target]])^2)  # Compute MSE for each target
  # return (mse)
  
  rmse <- sqrt(mse)
  
  # return (rmse)
  
  # Normalize MSE by the mean of the actual values
  target_mean <- mean(test_y[[target]])
  mse_normalized <- rmse / target_mean
  
  return (list(rmse, mse_normalized))  # Return the normalized MSE for this target
}

# Example usage for one target variable:
rf_rmse_CO <- rf_result("CO.GT.")        # 0.5778774   # 0.2807137
rf_rmse_C6H6 <- rf_result("C6H6.GT.")    # 3.601207    # 0.3714302
rf_rmse_NOx <- rf_result("NOx.GT.")      # 75.71713    # 0.3230885
rf_rmse_NO2 <- rf_result("NO2.GT.")      # 21.05513    # 0.1953094

# Display MSE for each target
rf_rmse_CO       # 0.4806054 0.2334622
rf_rmse_C6H6     # 2.900826  0.2991926
rf_rmse_NOx      # 67.9865   0.2901015
rf_rmse_NO2      # 20.07209  0.1861906

### random forest is better than KNN


################################################ K means Clustering

# df_normalized = as.data.frame(lapply(df[,-c(1, 2)], normalize))

# Ensure month is a factor with all levels present
df$month <- factor(df$month, levels = month.abb)  

# Assign colors explicitly to each month
month_colors <- setNames(rainbow(length(levels(df$month))), levels(df$month))
months_colors <- month_colors[df$month]  

# PCA analysis
pca_result <- prcomp(df[, -c(1, 2, 15)], center = TRUE, scale = TRUE)

# Proportion of variance
prcomp_proportionVariate <- pca_result$sdev^2 / sum(pca_result$sdev^2)
round(prcomp_proportionVariate, 5)

# Plot
plot(pca_result$x[, 1], pca_result$x[, 2], 
     xlab = "PC1(55.91%)", ylab = "PC2(19.76%)", 
     col = months_colors, pch = 19, 
     main = "PCA dimension reduction")

# Correct legend with all months labeled

legend("bottomright", legend = names(month_colors),
       col = month_colors, pch = 19, cex = 0.8)

# there is a pattern for months

## [1] 237 13 69 71 179
library(factoextra)

fviz_nbclust(df[, -c(1, 2, 15)], kmeans, method = "silhouette")

results <- kmeans(df[, -c(1, 2, 15)], centers = 2)
fviz_cluster(results, data = df[, -c(1, 2, 15)], geom = "point")

## no meaning for now

