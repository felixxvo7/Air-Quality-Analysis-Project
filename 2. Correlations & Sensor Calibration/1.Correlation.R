## -----------------------------------------------------------------------------------------------------------
# Load required libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)

data = read_csv("Documents/GitHub/Air-Quality-Analysis-Project/2. Correlations & Sensor Calibration/final_cleaned_data.csv")

summary(data[,c(-1, -2)])

## -----------------------------------------------------------------------------------------------------------
# Hist for each pollutant
par(mfrow=c(2,4))  # 2x2 layout for plots
hist(data$CO.GT., main="CO Distribution", col="skyblue")
hist(data$C6H6.GT., main="C6H6 Distribution", col="lightcoral")
hist(data$NO2.GT., main="NO2 Distribution", col="lightgreen")
hist(data$NOx.GT., main="NOx Distribution", col="gold")

boxplot(data$CO.GT., main="CO Distribution", col="skyblue")
boxplot(data$C6H6.GT., main="C6H6 Distribution", col="lightcoral")
boxplot(data$NO2.GT., main="NO2 Distribution", col="lightgreen")
boxplot(data$NOx.GT., main="NOx Distribution", col="gold")


## -----------------------------------------------------------------------------------------------------------
# Box plots for each pollutant
p1 = ggplot(data, aes(y=CO.GT.)) + 
  geom_boxplot(fill="skyblue") + 
  theme_minimal() + 
  ggtitle("Boxplot of CO (mg/m³)") + 
  ylab("CO (mg/m³)")

p2 = ggplot(data, aes(y=C6H6.GT.)) + 
  geom_boxplot(fill="lightcoral") + 
  theme_minimal() + 
  ggtitle("Boxplot of Benzene (C6H6) (µg/m³)") + 
  ylab("C6H6 (µg/m³)")

p3 = ggplot(data, aes(y=NO2.GT.)) + 
  geom_boxplot(fill="lightgreen") + 
  theme_minimal() + 
  ggtitle("Boxplot of NO2 (µg/m³)") + 
  ylab("NO2 (µg/m³)")

p4 = ggplot(data, aes(y=NOx.GT.)) + 
  geom_boxplot(fill="gold") + 
  theme_minimal() + 
  ggtitle("Boxplot of NOx (ppb)") + 
  ylab("NOx (ppb)")

# Arrange the boxplots in a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

## -----------------------------------------------------------------------------------------------------------
# Compute Spearman correlation matrix
cor_matrix = cor(data %>% select_if(is.numeric), method = "spearman", use = "complete.obs")
cor_matrix

# Convert to long format for visualization
cor_data = melt(cor_matrix)

# Heatmap visualization
ggplot(cor_data, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +  
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Reshape for heatmap
heatmap_data = cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("True_Pollutant") %>%
  filter(True_Pollutant %in% c("CO.GT.", "NOx.GT.", "NO2.GT.", "C6H6.GT.")) %>%
  select(True_Pollutant, 
         PT08.S1.CO., PT08.S3.NOx., PT08.S4.NO2., PT08.S2.NMHC., PT08.S5.O3.,
         T, RH, AH) %>%
  melt(id.vars = "True_Pollutant", 
       variable.name = "Sensor_Env", 
       value.name = "Correlation")

# Create heatmap
ggplot(heatmap_data, aes(x = Sensor_Env, y = True_Pollutant, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1),
    name = "Correlation (ρ)"
  ) +
  labs(
    title = "True Pollutants vs Sensor Response & Environment",
    subtitle = "Spearman correlation coefficients",
    x = "Sensors & Environmental Factors",
    y = "Reference Measurements (Ground Truth)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

