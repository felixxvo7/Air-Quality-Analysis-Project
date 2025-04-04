
library(tidyverse)
library(patchwork)
library(ggplot2)
library(infer)

data = read_csv("Documents/GitHub/Air-Quality-Analysis-Project/2. Correlations & Sensor Calibration/final_cleaned_data.csv") 
# Handle NAs and scale for numerical stability
data_clean <- data %>%
  drop_na(PT08.S1.CO., CO.GT.) %>%
  mutate(
    PT.CO.scale = scale(PT08.S1.CO.),
    CO.scale = scale(CO.GT.)
  )

# Permutation test
observed_stat <- data_clean %>% 
  specify(PT.CO.scale ~ CO.scale) %>% 
  calculate(stat = "correlation", method = "spearman")

null_dist <- data_clean %>%
  specify(PT.CO.scale ~ CO.scale) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "correlation", method = "spearman")

# Continuity correction to avoid p=0
p_value <- (sum(abs(null_dist$stat) >= abs(observed_stat$stat)) + 1) / (1000 + 1)

## -----------------------------------------------------------------------------------------------------------
set.seed(1)
# Define matching pairs
pairs = list(
  list(sensor = "PT08.S1.CO.", ref = "CO.GT."),
  list(sensor = "PT08.S3.NOx.", ref = "NOx.GT."),
  list(sensor = "PT08.S4.NO2.", ref = "NO2.GT.")
)

# Permutation test function
perm_test = function(x, y, n_perm = 1000) {
  
  # Observed correlation
  r_obs = cor(x, y, method = "spearman")
  
  # Null distribution
  r_perm = replicate(n_perm, cor(sample(x), sample(y), method = "spearman",use = "pairwise.complete.obs"))
  
  # Empirical p-value (two-sided)
  p_val = mean(abs(r_perm) >= abs(r_obs))
  
  return(list(correlation = r_obs, p_value = p_val))
}

# Run the test for each pair
results = lapply(pairs, function(pair) {
  sensor = pair$sensor
  ref = pair$ref
  test_result = perm_test(data[[sensor]], data[[ref]])
  data.frame(
    Sensor = sensor,
    Reference = ref,
    Correlation = test_result$correlation,
    P_Value = test_result$p_value
  )
})

# Combine results
final_results = do.call(rbind, results)
final_results


## -----------------------------------------------------------------------------------------------------------
# Create scatterplots with correlation coefficients
scatter_plots <- lapply(pairs, function(pair) {
  sensor <- pair$sensor
  ref <- pair$ref
  
  ggplot(data, aes(x = .data[[sensor]], y = .data[[ref]])) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste(sensor, "vs", ref),
         x = sensor,
         y = ref) +
    theme_minimal()
})

# Combine with permutation plots
full_plots <- lapply(1:length(pairs), function(i) {
  scatter_plots[[i]] + results[[i]]$plot
})

# Display all plots
wrap_plots(full_plots, ncol = 2) +
  plot_annotation(title = "Sensor-Reference Relationships and Permutation Tests",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))


obs_statistic <- gss %>%
  specify(age ~ partyid) %>%
  calculate(stat = "F")
# generate the null distribution with randomization
null_dist <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")


