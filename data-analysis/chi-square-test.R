# Get the list of variable names
variable_names <- names(categorical_df)

# Initialize a dataframe for the results
chi_square_results <- data.frame(var1 = character(), 
                                 var2 = character(), 
                                 p_value = numeric(), 
                                 stringsAsFactors = FALSE)

# Chi-square test
chi_square_test <- function(var1, var2, data) {
  # Create a contingency table
  table <- table(data[[var1]], data[[var2]])

  # Perform Chi-square test
  test <- chisq.test(table, correct = FALSE)
  p.value <- test$p.value
  
  # Add reuslts to chi_square_results dataframe
  new_row <- data.frame(var1 = var1, var2 = var2, p_value = p.value, stringsAsFactors = FALSE)
  chi_square_results <<- rbind(chi_square_results, new_row)
  
  return(p.value)
}

for (i in 1:length(variable_names)) {
  for (j in 1:length(variable_names)) {
    chi_square_test(variable_names[i], 
                    variable_names[j], 
                    categorical_df)
  }
}

head(chi_square_results)

chi_square_results_CONCOHORT <- chi_square_results %>% 
  filter(var1 == "CONCOHORT") %>% 
  mutate(color = ifelse(p_value > 0.10, "Insignificant Association", "Significant Association"))

chi_square_results_CONCOHORT_filtered <- chi_square_results_CONCOHORT %>% 
  filter(p_value <= 0.10)

# Plot the results
chi_square_results_plot <- ggplot(data = chi_square_results_CONCOHORT, 
       aes(x = var1, y = p_value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(col = color), 
             position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = 0.10, color = "red", linetype = "dashed") +
  
  theme_classic() +
  scale_color_manual(values = c("Insignificant Association" = "red", 
                                "Significant Association" = "black")) +
  labs(y = "Chi-square p-value", 
       x = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_blank())

ggsave("figures/pre-analysis/chi_square_results.png", 
       plot = chi_square_results_plot,
       width = 3, height = 5, dpi = 1000)

# Filter the strongly associated categorical variables from the original dataframe
categorical_vars_selected <- chi_square_results_CONCOHORT_filtered$var2
categorical_vars_selected <- setdiff(categorical_vars_selected, "CONCOHORT")
categorical_vars_selected <- c("PATNO", categorical_vars_selected)

categorical_df_selected <- categorical_df[, categorical_vars_selected, drop = FALSE]
