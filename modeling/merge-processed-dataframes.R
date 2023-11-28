# Merge only categorical variables
categorical_df <- merge(updated_binomial_selected_df, # Binary predictors
                  updated_ordered_df_selected, # Ordered predictors
                  by = "PATNO", all = TRUE)
categorical_df <- merge(categorical_df, 
                  updated_qualitative_df_selected,  # Qualitative predictors
                  by = "PATNO", all = TRUE)
categorical_df <- merge(categorical_df,
                  response_var_df, # Concohort category (response)
                  by = "PATNO", all = TRUE)

categorical_df <- lapply(categorical_df, factor)
categorical_df <- as.data.frame(categorical_df)

## categorical_df was used in chi-square test
## Variables with significant association to CONCOHORT were returned in categorical_df_selected

# Merge all dataframes for modeling
model_df <- merge(categorical_df_selected, # All categorical predictors
                  updated_numeric_df_selected, # Numeric predictors (standardized)
                  by = "PATNO", all = TRUE)
model_df <- merge(model_df,
                  response_var_df, # CONCOHORT (response)
                  by = "PATNO", all = TRUE)