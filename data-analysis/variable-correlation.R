# Numeric dataframe for correlation: updated_numeric_df_selected
# updated_numeric_df_selected was already filtered with ANOVA test
cor_df <- updated_numeric_df_selected %>% 
  lapply(., as.numeric) %>% 
  as.data.frame() %>% 
  mutate(X0_2__di_22_6_BMP = NULL,
         total_di_22_6_BMP = NULL) %>% 
  na.omit()

# Compute correlation coefficient
pearson_cor <- cor(cor_df, method = "pearson")

# Plot in correlation matrix
png("figures/pre-analysis/pearson_correlation.png",
    width = 15000, height = 15000, res = 1000)
pearson_cor_plot <- corrplot.mixed(pearson_cor, 
                                   upper = "number", upper.col = viridis(20, option = "C"),
                                   lower = "pie", lower.col = viridis(20, option = "C"), 
                                   tl.pos = "d",
                                   tl.col = "black")
dev.off()

# Remove variables and create new correlation plot
cor_df_updated <- cor_df %>% 
  dplyr::select(-MSEADLG, -scopa_gi, -pigd)

pearson_cor_updated <- cor(cor_df_updated, method = "pearson")
corrplot.mixed(pearson_cor_updated, 
               upper = "number", upper.col = viridis(20, option = "C"),
               lower = "pie", lower.col = viridis(20, option = "C"), 
               tl.pos = "d",
               tl.col = "black")

selected_vars <- unique(names(cor_df_updated))

# Create the final filtered numeric dataframe
final_numeric_df_selected <- updated_numeric_df_selected[, selected_vars, drop = FALSE] %>% 
  mutate(PATNO = as.character(PATNO))

# # Plot variables selected by correlation
# plot_numeric_df <- cor_results_df %>% 
#   filter(!duplicated(cor_coef) | duplicated(cor_coef, fromLast = TRUE)) %>% 
#   mutate(color = ifelse(-critical_cor < cor_coef & critical_cor > cor_coef, 
#                         "Weak Correlation", "Strong Correlation"))
# 
# # Plot the results
# numeric_correlation_plot <- ggplot(data = plot_numeric_df, aes(x = 1, y = cor_coef)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_point(aes(col = color), 
#              position = position_jitter(width = 0.2)) +
#   geom_hline(yintercept = critical_cor, color = "darkred", linetype = "dashed", lwd = 1) +
#   geom_text(aes(x = Inf, y = critical_cor, label = paste("Critical r = ", format(critical_cor, digits = 3))), 
#             vjust = 1.5, hjust = 1, color = "darkred", size = rel(3)) +
#   
#   theme_classic() +
#   scale_y_log10() +
#   scale_color_manual(values = c("Strong Correlation" = "red", 
#                                 "Weak Correlation" = "black")) +
#   labs(y = "Correlation Coefficient", 
#        x = NULL) +
#   theme(legend.position = "none", 
#         axis.text.x = element_blank())
# 
# ggsave("figures/pre-analysis/numeric_correlation.png", 
#        plot = numeric_correlation_plot,
#        width = 5, height = 5, dpi = 1000)
