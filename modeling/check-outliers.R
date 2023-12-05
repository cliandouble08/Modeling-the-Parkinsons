# Dataframe to check outliers for: model_df_na_free
count_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- column < lower_bound | column > upper_bound
  return(sum(outliers))
}

outlier_counts <- data.frame(variable = colnames(model_df_na_free))
outlier_counts$total_count <- sapply(model_df_na_free, function(column) nrow(model_df_na_free))

outlier_counts$outlier_count <- sapply(model_df_na_free, count_outliers)
outlier_counts$outlier_proportion <- outlier_counts$outlier_count / outlier_counts$total_count