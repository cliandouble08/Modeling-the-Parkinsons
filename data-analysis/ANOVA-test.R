numeric_df_selected_CONCOHORT <- merge(numeric_df_selected, response_var_df, by = "PATNO")
numeric_df_selected_CONCOHORT[] <- lapply(numeric_df_selected_CONCOHORT, as.numeric)
numeric_df_selected_CONCOHORT$CONCOHORT <- as.factor(numeric_df_selected_CONCOHORT$CONCOHORT)

# Initialize a dataframe for the ANOVA results
anova_results <- data.frame(
  Variable = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Perform ANOVA for each variable
for (variable in setdiff(names(numeric_df_selected_CONCOHORT), "CONCOHORT")) {
  anova_result <- aov(as.formula(paste(variable, " ~ CONCOHORT")), data = numeric_df_selected_CONCOHORT)
  summary_result <- summary(anova_result)
  p_value <- summary_result[[1]]$`Pr(>F)`[1]
  
  new_row <- data.frame(Variable = variable, p_value = p_value, stringsAsFactors = FALSE)
  anova_results <- rbind(anova_results, new_row)
}

# Test confidence interval for the p-values
t_test <- t.test(anova_results$p_value)
upper_limit <- t_test$conf.int[2]
# Still very large upper limit because of the almost flat distribution of p-value
plot(anova_results$p_value)

# Calculate the median of p-values
summary_pvalue <- summary(anova_results$p_value)
first_qua <- summary_pvalue["1st Qu."]

# Filter for significant associations (i.e., with p-value smaller than the upper_limit)
significant_anova_results <- anova_results %>%
  filter(p_value <= first_qua)

# Display the ANOVA results
head(anova_results)

# Display significant ANOVA results
head(significant_anova_results)

# Filter the original numeric dataframe by the results
numeric_vars_selected <- significant_anova_results$Variable
numeric_vars_selected <- setdiff(numeric_vars_selected, "CONCOHORT")

updated_numeric_df_selected <- numeric_df_selected[, numeric_vars_selected, drop = FALSE]
updated_numeric_df_selected <- as.data.frame(sapply(updated_numeric_df_selected, as.numeric))

# Standardize all columns except PATNO
standardize_cols <- names(updated_numeric_df_selected) != "PATNO"

updated_numeric_df_selected[, standardize_cols] <- scale(updated_numeric_df_selected[, standardize_cols])