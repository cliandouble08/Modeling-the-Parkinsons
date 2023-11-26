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

# Difference between age_at_visit and agediag, ageonset, age_LP, age_DATSCAN, age_upsit
df <- df %>%
  mutate(across(c(agediag, ageonset, age_LP, age_DATSCAN, age_upsit), ~na_if(.x, ".")))

df_age_difference <- df %>% 
  mutate(
    # Convert columns to numeric
    agediag = as.numeric(agediag),
    ageonset = as.numeric(ageonset),
    age_LP = as.numeric(age_LP),
    age_DATSCAN = as.numeric(age_DATSCAN),
    age_upsit = as.numeric(age_upsit),
    
    # Calculate differences
    age_at_visit_agediag = ifelse(is.na(agediag), NA, age_at_visit - agediag),
    age_at_visit_ageonset = ifelse(is.na(ageonset), NA, age_at_visit - ageonset),
    age_at_visit_LP = ifelse(is.na(age_LP), NA, age_at_visit - age_LP),
    age_at_visit_DATSCAN = ifelse(is.na(age_DATSCAN), NA, age_at_visit - age_DATSCAN),
    age_at_visit_upsit = ifelse(is.na(age_upsit), NA, age_at_visit - age_upsit)
  )

df_age_difference_long <- df_age_difference %>%
  pivot_longer(
    cols = starts_with("age_at_visit_"),
    names_to = "variable",
    values_to = "value"
  )

violin_age_difference <- ggplot(data = df_age_difference_long) +
  geom_violin(aes(x = variable, y = value, fill = variable)) +
  geom_hline(yintercept = 10, linetype = "dashed", 
             col = "darkred", lwd = 1, alpha = 0.5) +
  theme_classic() +
  scale_y_log10() +
  labs(y = "Age Difference from Age at Visit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank(), 
        legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5)

ggsave("figures/pre-analysis/violin_age_difference_from_age_of_visit.png", 
       plot = violin_age_difference,
       width = 5, height = 5, dpi = 1000)