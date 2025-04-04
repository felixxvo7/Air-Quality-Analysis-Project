library(tidyverse)


raw_data = read_csv("Documents/GitHub/Air-Quality-Analysis-Project/Datasets/formated_data.csv")

raw_data[raw_data] <- 0

summary(raw_data[,c(-1, -2)])

clean_data = read_csv("Documents/GitHub/Air-Quality-Analysis-Project/Datasets/final_cleaned_data.csv")

summary(clean_data[,c(-1, -2)])
