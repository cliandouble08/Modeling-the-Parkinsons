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
  deg.freedom <- test$parameter
  chi.square <- test$statistic
  p.value <- test$p.value
  
  # Add reuslts to chi_square_results dataframe
  new_row <- data.frame(var1 = var1, var2 = var2, degree_of_freedom = deg.freedom, chi_square = chi.square, p_value = p.value, stringsAsFactors = FALSE)
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
  filter(var1 == "CONCOHORT")

# Determine the confidence interval for p-values
t_test <- t.test(chi_square_results_CONCOHORT_filtered$p_value)
upper_limit <- t_test$conf.int[2]

# Mark variables with a p-value smaller than the upper_limit as "significant association"
chi_square_results_CONCOHORT <- chi_square_results_CONCOHORT %>% 
  mutate(color = ifelse(p_value > upper_limit, "Insignificant Association", "Significant Association"))

# Select variables with a p-value smaller than the upper_limit (i.e., significant association to CONCOHORT)
chi_square_results_CONCOHORT_filtered <- chi_square_results_CONCOHORT %>% 
  filter(p_value <= upper_limit)

# Plot the results
chi_square_results_plot <- ggplot(data = chi_square_results_CONCOHORT, 
       aes(x = var1, y = p_value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(col = color), 
             position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = upper_limit, color = "darkred", linetype = "dashed", lwd = 1) +
  geom_text(aes(x = Inf, y = upper_limit, label = paste("p-value = ", format(upper_limit, scientific = TRUE, digits = 3))), 
            vjust = 1.5, hjust = 1, color = "darkred", size = rel(3)) +
  
  theme_classic() +
  scale_y_log10() +
  scale_color_manual(values = c("Insignificant Association" = "red", 
                                "Significant Association" = "black")) +
  labs(y = "Chi-square p-value", 
       x = "Association to `CONCOHORT`") +
  theme(legend.position = "none", 
        axis.text.x = element_blank())

ggsave("figures/pre-analysis/chi_square_results.png", 
       plot = chi_square_results_plot,
       width = 5, height = 5, dpi = 1000)

# Filter the strongly associated categorical variables from the original dataframe
categorical_vars_selected <- chi_square_results_CONCOHORT_filtered$var2
categorical_vars_selected <- setdiff(categorical_vars_selected, "CONCOHORT")
categorical_vars_selected <- c("PATNO", categorical_vars_selected)

categorical_df_selected <- categorical_df[, categorical_vars_selected, drop = FALSE]