# Before any correlation test, add 1 to all CONCOHORT values
# CONCOHORT originally has two levels of 0 and 1
cor_df <- model_df %>% 
  mutate(CONCOHORT = CONCOHORT + 1)

# Kendall's tau correlation
kendall_cor <- cor(cor_df, method = "kendall")

# Filter variables that have non-NA correlation with the other

# Identify non-NA correlations
non_na_kendall_cor <- !is.na(kendall_cor)

# Remove diagonal values becasue the diagonal will always be 1
diag(non_na_kendall_cor) <- FALSE

# Extract variables with non-NA (TRUE) correlations
cor_vars <- rownames(kendall_cor)[apply(non_na_kendall_cor, 1, any)]

# Create a new cor_df with only correlated variables
filtered_cor_df <- cor_df[, cor_vars, drop = FALSE]

# New Kendall's tau coefficient calculated
kendall_cor_filtered <- cor(filtered_cor_df, method = "kendall")

png("figures/correlation/kendalls-tau-filtered.png", 
    width = 15000, height = 15000, res = 1500)
kendall_cor_filtered_plot <- corrplot.mixed(kendall_cor_filtered, 
               upper = "number", upper.col = viridis(10, option = "C"),
               lower = "square", lower.col = viridis(10, option = "C"), 
               tl.pos = "d",
               tl.col = "black")
dev.off()
