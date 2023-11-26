# Chi-square test for qualitative variables
qualitative_association_df <- lapply(qualitative_df_selected, factor)
qualitative_association_df <- as.data.frame(qualitative_df_selected)