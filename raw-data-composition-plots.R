# Density plot for na_proportion
density_plot_na_proportion <- ggplot(data = naCount_raw_df) +
  geom_density(aes(x = na_proportion), lwd = 0.8) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  xlim(0, 1) +
  labs(x = "Proportion of NA values", 
       y = "Density") +
  theme_classic()

ggsave("figures/pre-analysis/density-na-proportion.png", 
       plot = density_plot_na_proportion,
       width = 5, height = 5, dpi = 1000)

# Class of each column
# Get the class of each column
column_classes <- sapply(df, class)

# Create a new dataframe with the column number, name, and class
df_column_class <- data.frame(
  column_number = seq_along(df),
  column_name = names(df),
  class = column_classes,
  stringsAsFactors = FALSE, 
  row.names = NULL
)

# Summarize the number of columns for each class
df_column_class_summary <- df_column_class %>%
  group_by(class) %>% 
  summarise(count = n()) %>% 
  mutate(class_proportion = count / sum(count), 
         ymax = cumsum(class_proportion), 
         ymin = c(0, head(ymax, n = -1)), 
         label_position = (ymax + ymin) / 2, 
         label = paste0(class, "\n", percent(class_proportion, accuracy = 0.01)))

# Create a pie chart for df_column_class summary
pie_chart_column_class <- ggplot(df_column_class_summary, 
       aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = class)) +
  geom_rect() +
  geom_text(x = 2, 
            aes(y = label_position, 
                label = label),
            color = "black", 
            size = 3) +
  coord_polar(theta = "y") +
  xlim(1, 4) +
  labs(fill = "Column Class", 
       y = "Class Proportion", 
       x = "") +
  scale_fill_viridis(discrete = TRUE) +
  theme_classic() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

ggsave("figures/pre-analysis/pie_column_class.png", 
       plot = pie_chart_column_class,
       width = 5, height = 5, dpi = 1000)
