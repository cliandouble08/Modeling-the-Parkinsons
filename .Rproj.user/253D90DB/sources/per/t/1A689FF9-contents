# Before any correlation test, add 1 to all CONCOHORT values
# CONCOHORT originally has two levels of 0 and 1
cor_df[] <- lapply(updated_numeric_df_selected, as.numeric) %>% 
  na.omit()

# Calculate correlation
pearson_cor <- cor(cor_df, method = "pearson")

# Filter variables that have non-NA correlation with the other

# Identify non-NA correlations
non_na_pearson_cor <- !is.na(pearson_cor)

# Remove diagonal values becasue the diagonal will always be 1
diag(non_na_pearson_cor) <- FALSE

# Extract variables with non-NA (TRUE) correlations
cor_vars <- rownames(pearson_cor)[apply(non_na_pearson_cor, 1, any)]

# Create a new cor_df with only correlated variables
filtered_cor_df <- cor_df[, cor_vars, drop = FALSE]

# New Kendall's tau coefficient calculated
pearson_cor_filtered <- cor(filtered_cor_df, method = "pearson")

filtered_cor_values <- as.vector(pearson_cor_filtered[upper.tri(pearson_cor_filtered)])

filtered_cor_df <- as.data.frame(filtered_cor_values)
filtered_cor_t <- t.test(filtered_cor_df$filtered_cor_values)

lower_limit <- filtered_cor_t$conf.int[1]
upper_limit <- filtered_cor_t$conf.int[2]

png("figures/correlation/pearson_correlation_filtered.png",
    width = 15000, height = 15000, res = 1500)
pearson_cor_filtered_plot <- corrplot.mixed(pearson_cor_filtered, 
                                            upper = "number", upper.col = viridis(10, option = "C"),
                                            lower = "square", lower.col = viridis(10, option = "C"), 
                                            tl.pos = "d",
                                            tl.col = "black")
dev.off()

# Strong correlation between: 
filtered_cor_values <- as.vector(pearson_cor_filtered[upper.tri(pearson_cor_filtered)])

filtered_cor_df <- as.data.frame(filtered_cor_values)
filtered_cor_t <- t.test(filtered_cor_df$filtered_cor_values)

lower_limit <- filtered_cor_t$conf.int[1]
upper_limit <- filtered_cor_t$conf.int[2]

