# Density plot for na_proportion
ggplot(data = naCount_raw_df) +
  geom_density(aes(x = na_proportion), lwd = 0.8) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  xlim(0, 1) +
  labs(x = "Proportion of NA values", 
       y = "Density") +
  theme_classic()