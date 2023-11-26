# Merge updated dataframes into one for modeling
model_df <- merge(updated_binomial_selected_df, 
                  updated_ordered_df_selected, 
                  by = "PATNO", all = TRUE)
model_df <- merge(model_df, 
                  updated_qualitative_df_selected, 
                  by = "PATNO", all = TRUE)
model_df <- merge(model_df, 
                  response_var_df, 
                  by = "PATNO", all = TRUE)