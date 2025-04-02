setwd("C:/Users/DucDo/OneDrive-UniversityofManitoba/Documents/University_documents/UofM/WInter2025/DATA2010/Air-Quality-Analysis-Project")

df = read.csv("final_cleaned_data.csv")

df_AQI = read.csv("Sensor Calibration/data_with_AQI.csv")

df$Date <- as.Date(df$Date)
df$month <- format(df$Date, "%m")  # Extract month as number (01, 02, ...)

categorize_AQI = function(AQI) {
  if (AQI <= 50) {
    return("Good")
  } else if (AQI <= 100) {
    return("Moderate")
  } else if (AQI <= 150) {
    return("Unhealthy for Sensitive Groups")
  } else if (AQI <= 200) {
    return("Unhealthy")
  } else if (AQI <= 300) {
    return("Very Unhealthy")
  } else {
    return("Hazardous")
  }
}

df$AQI = df_AQI$AQI
# Apply AQI function to each row in dataset
df = df %>%
  mutate(AQI_Category = sapply(AQI, categorize_AQI))

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

df$month = as.integer(df$month)

# Define predictors (exclude Date, Time, and targets)
predictors <- df %>% select(-c(Date, Time, CO.GT., C6H6.GT., NOx.GT., NO2.GT., AQI, AQI_Category))
# predictors <- df %>% select(-c(Date, Time, NO2.GT.))

# Define multiple target variables (CO, NOx, Benzene)
targets <- df %>% select(CO.GT., C6H6.GT., NOx.GT., NO2.GT.)
# targets <- df %>% select(NO2.GT.)

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
                        # y = train_y,
                        y = train_y[[target]],
                        k = 5)  # k = 5 nearest neighbors
  
  # Predictions
  predictions <- knn_fit$pred
  
  # Evaluate model performance for multiple targets
  mse <- mean((predictions - test_y[[target]])^2)  # Compute MSE for each target
  # mse <- mean((predictions - test_y)^2)
  # return (mse)
  
  rmse <- sqrt(mse)
  
  # return (rmse)
  
  # Normalize MSE by the mean of the actual values
  target_mean <- mean(test_y[[target]])
  # target_mean <- mean(test_y)
  
  mse_normalized <- rmse / target_mean
  
  return (list(rmse, mse_normalized))  # Return the normalized MSE for this target
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


###=========================================
# use T, AH and RH reduces the rmse
# > rmse_CO
# [1] 0.2595928
# > rmse_C6H6
# [1] 0.2542076
# > rmse_NOx
# [1] 0.2914516
# > rmse_NO2
# [1] 0.17697

# use month reduces rmse even more
# > rmse_CO
# [1] 0.4978949
# [1] 0.2418609
# > rmse_C6H6
# [1] 1.839073
# [1] 0.1896829
# > rmse_NOx
# [1] 59.58025
# [1] 0.2542317
# > rmse_NO2
# [1] 17.60785
# [1] 0.1633321


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
  rf_model <- randomForest(as.formula(paste(target, "~ PT08.S1.CO. + PT08.S2.NMHC. + PT08.S3.NOx. + PT08.S4.NO2. + PT08.S5.O3. + T + RH + AH + month")),  
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

# using T, AH, RH and month reduces rmse like knn
# > rf_rmse_CO      
# [1] 0.4385208
# [1] 0.2130189
# > rf_rmse_C6H6    
# [1] 1.562383
# [1] 0.161145
# > rf_rmse_NOx      
# [1] 55.14515
# [1] 0.2353069
# > rf_rmse_NO2     
# [1] 16.7216
# [1] 0.1551112

################################################ K means Clustering

# df_normalized = as.data.frame(lapply(df[,-c(1, 2)], normalize))

df$month <- factor(df$month, levels = sprintf("%02d", 1:12))  # Ensure proper factor levels with leading zero  

# Assign colors explicitly to each month
month_colors <- setNames(rainbow(length(levels(df$month))), levels(df$month))
months_colors <- month_colors[df$month]

df$month = as.integer(df$month)
# PCA analysis
# pca_result <- prcomp(df[, -c(1, 2, 15, 16)], center = TRUE, scale = TRUE)
pca_result <- prcomp(df[, c(4, 6, 8, 10, 11, 12, 13, 14, 15)], center = TRUE, scale = TRUE)


# Proportion of variance
prcomp_proportionVariate <- pca_result$sdev^2 / sum(pca_result$sdev^2)
round(prcomp_proportionVariate, 5)

# Plot
plot(pca_result$x[, 1], pca_result$x[, 2], 
     xlab = "PC1(53.42%)", ylab = "PC2(23.88%)", 
     col = months_colors, pch = 19, 
     main = "PCA dimension reduction")

# Correct legend with all months labeled

legend("bottomright", legend = names(month_colors),
       col = month_colors, pch = 19, cex = 0.5)

# there is a pattern for months

## [1] 237 13 69 71 179
library(factoextra)

fviz_nbclust(df[, c(4, 6, 8, 10, 11, 12, 13, 14, 15)], kmeans, method = "silhouette")

results <- kmeans(df[, c(4, 6, 8, 10, 11, 12, 13, 14, 15)], centers = 2)
fviz_cluster(results, data = df[, c(4, 6, 8, 10, 11, 12, 13, 14, 15)], geom = "point")

## no meaning for now (for both month included or excluded)

##________________________________________________________________ AQI



df$AQI_Category <- as.factor(df$AQI_Category)  # Ensure it's a factor

# Assign colors explicitly to each month
AQI_colors <- setNames(rainbow(length(levels(df$AQI_Category))), levels(df$AQI_Category))
AQIs_colors <- AQI_colors[df$AQI_Category]

# # Plot
# plot(pca_result$x[, 1], pca_result$x[, 2], 
#      xlab = "PC1(55.91%)", ylab = "PC2(19.76%)", 
#      col = AQIs_colors, pch = 19, 
#      main = "PCA dimension reduction")
# 
# # Correct legend with all months labeled
# 
# legend("bottomright", legend = names(AQI_colors),
#        col = AQI_colors, pch = 19, cex = 0.8)

library(ggplot2)

# Create a data frame for ggplot
pca_df <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], AQI_Category = df$AQI_Category)

# Plot using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2, color = AQI_Category)) +
  geom_point(size = 3) +
  labs(title = "PCA dimension reduction", 
       x = "PC1 (47.71%)", y = "PC2 (22.91%)") +
  theme_minimal() +
  theme(legend.position = "right")  # Easily place the legend on the right

###================================================ KNN for AQI category 
library(class)

# y_pred <- knn(train = train[,-(10:13)], test = test_x, cl = df[,5], k = 5)

AQI_cat_pred <- knn(train = train_x, test = test_x, cl = df[trainIndex, 17], k = 1)
AQI_cat_pred = as.factor(AQI_cat_pred)

AQI_cat_actual <- df[-trainIndex, 17]
table(AQI_cat_actual, AQI_cat_pred)

# Install ggplot2 if not already installed
# install.packages("ggplot2")

# library(ggplot2)
# 
# # Create the confusion matrix
# cm <- table(AQI_cat_actual, AQI_cat_pred)
# 
# # Convert the confusion matrix to a data frame
# cm_df <- as.data.frame(cm)
# 
# # Plot the confusion matrix as a heatmap
# ggplot(cm_df, aes(x = AQI_cat_pred, y = AQI_cat_actual, fill = Freq)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   labs(x = "Predicted Category", y = "Actual Category", fill = "Frequency") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ###### proportion
# 
# # Normalize by the total count to get proportions
# cm_df$Proportion <- cm_df$Freq / sum(cm_df$Freq)
# 
# # Plot the confusion matrix as a heatmap with proportions
# ggplot(cm_df, aes(x = AQI_cat_pred, y = AQI_cat_actual, fill = Proportion)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   labs(x = "Predicted Category", y = "Actual Category", fill = "Proportion") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare actual and predicted values
correct_predictions <- sum(AQI_cat_actual == AQI_cat_pred)

# Total number of predictions
total_predictions <- length(AQI_cat_actual)

# Accuracy
accuracy <- correct_predictions / total_predictions
accuracy     # 0.6762821 # better than k = 8, 10
# 0.6901709 for k = 3   # better than k = 2
# 0.6987179 for k = 1

###============================ RF for AQI cat

rf_AQI_cat <- randomForest(AQI_Category ~ ., data=data.frame(train_x, AQI_Category = as.factor(df[trainIndex,]$AQI_Category)))
rf_AQI_cat_pred <- predict(rf_AQI_cat, test_x)
table(rf_AQI_cat_pred, AQI_cat_actual)

correct_predictions <- sum(AQI_cat_actual == rf_AQI_cat_pred)

# Total number of predictions
total_predictions <- length(AQI_cat_actual)

# Accuracy
accuracy <- correct_predictions / total_predictions
accuracy # 0.7195513


##### plot

library(ggplot2)
library(reshape2)

# conf_df <- as.data.frame(table(rf_AQI_cat_pred, AQI_cat_actual))
conf_df <- as.data.frame(table(AQI_cat_pred, AQI_cat_actual))

names(conf_df) <- c("Predicted", "Actual", "Freq")

###
prop_by_actual <- conf_df %>%
  group_by(Actual) %>%
  mutate(Proportion = Freq / sum(Freq))

# Plot counts
p1 <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", name = "Count") +
  geom_text(aes(label = Freq), size = 3.5) +
  labs(title = "Confusion Matrix KNN k=1(Counts)",
       x = "Actual Category",
       y = "Predicted Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot proportions by actual category
p2 <- ggplot(prop_by_actual, aes(x = Actual, y = Predicted, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", 
                      name = "Proportion",
                      labels = scales::percent) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 1)), size = 3.5) +
  labs(title = "Confusion Matrix KNN k=1(Proportions by Actual Category)",
       x = "Actual Category",
       y = "Predicted Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate proportions by predicted category
prop_by_predicted <- conf_df %>%
  group_by(Predicted) %>%
  mutate(Proportion = Freq / sum(Freq))

# Plot proportions by predicted category
p3 <- ggplot(prop_by_predicted, aes(x = Actual, y = Predicted, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", 
                      name = "Proportion",
                      labels = scales::percent) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 1)), size = 3.5) +
  labs(title = "Confusion Matrix KNN k=1(Proportions by Predicted Category)",
       x = "Actual Category",
       y = "Predicted Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# To display all three plots together
# library(gridExtra)
# grid.arrange(p1, p2, p3, ncol = 3)
# Or if you prefer a vertical layout:
# grid.arrange(p1, p2, p3, nrow = 3)

################ RF is clearly better (best for Unhealthy and UnhealthyFSG with 79% and 74%). Make sense as those are the majorities of the dataset.
