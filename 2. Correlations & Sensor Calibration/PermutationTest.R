data = read.csv("./final_cleaned_data.csv")


library(patchwork)

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
  r_perm = replicate(n_perm, cor(x, sample(y), method = "spearman"))
  
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

