updated_numeric_df_selected_CONCOHORT <- merge(updated_numeric_df_selected, response_var_df, by = "PATNO")
updated_numeric_df_selected_CONCOHORT$CONCOHORT <- updated_numeric_df_selected_CONCOHORT$CONCOHORT + 1
updated_numeric_df_selected_CONCOHORT <- na.omit(updated_numeric_df_selected_CONCOHORT)

fit <- lm(CONCOHORT ~ ., data = updated_numeric_df_selected_CONCOHORT)
stepwise_model <- step(fit, direction = "both", step = 100000000)

best_subset_model <- regsubsets(CONCOHORT ~ ., data = updated_numeric_df_selected_CONCOHORT, nvmax = 20)

best_subset_results <- summary(best_subset_model)
data.frame(
  Adj.R2 = which.max(best_subset_results$adjr2),
  CP = which.min(best_subset_results$cp),
  BIC = which.min(best_subset_results$bic)
)
