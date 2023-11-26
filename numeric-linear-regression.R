# Merge the numeric dataframe with CONCOHORT (response) by matching PATNO
modeling_numeric_df_selected <- merge(updated_numeric_df_selected, response_var_df, by = "PATNO")

numeric_model <- lm(CONCOHORT ~ ., data = modeling_numeric_df_selected)
summary(numeric_model)