# Summarize the frequency for each CONCOHORT
sample_no <- nrow(response_var_df)
response_var_summary <- response_var_df %>% 
  group_by(CONCOHORT) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = count / sample_no, 
         cum_proportion = cumsum(proportion), 
         # cum_proportion_diff = c(0, diff(cum_proportion)),
         log_cum_odd = log(cum_proportion / (1 - cum_proportion)))

# Description of frequency overlay with proportions
max_count <- max(response_var_summary$count)
update_response_var_summary <- response_var_summary %>% 
  mutate(CONCOHORT = CONCOHORT + 1)

ggplot(data = update_response_var_summary) +
  geom_bar(aes(x = CONCOHORT, y = count), 
           stat = "identity", alpha = 0.1) +
  
  geom_point(aes(x = CONCOHORT, y = proportion * max_count, col = "Proportion"), size = 3) +
  geom_line(aes(x = CONCOHORT, y = proportion * max_count, col = "Proportion"), linewidth = 1) +
  
  # geom_point(aes(x = CONCOHORT, y = cum_proportion * max_count, col = "Cumulative Proportion"), size = 3) +
  # geom_line(aes(x = CONCOHORT, y = cum_proportion * max_count, col = "Cumulative Proportion"), linewidth = 1) +
  
  theme_classic() +
  scale_color_viridis_d(name = element_blank()) +
  scale_y_continuous(name = "Frequency", 
                     sec.axis = sec_axis(~. / max_count, name = "Proportion (%)")) +
  theme(legend.position = "bottom") +
  labs(x = "Cohort")
ggsave(file = "figures/modeling/data-description/cumulative-proportion.png", 
       last_plot(), 
       width = 5, height = 5, dpi = 1000)
dev.off()

log_cum_odds_plot <- ggplot(data = update_response_var_summary) +
  geom_point(aes(x = CONCOHORT, y = log_cum_odd), stat = "identity", size = 3) +
  geom_line(aes(x = CONCOHORT, y = log_cum_odd), lwd = 1) +
  
  theme_classic() +
  ylim(-2, 2) +
  labs(x = "Cohort", 
       y = "Log(Cumulative Odds)")
ggsave(file = "figures/modeling/data-description/log_cumulative_odds.png", 
       last_plot(), 
       width = 5, height = 5, dpi = 1000)
dev.off()

# Overlay with the likelihood for each concohort
diff_response_var_summary <- response_var_summary %>% 
  arrange(CONCOHORT) %>% 
  mutate(last_cum_proportion = ifelse(is.na(lag(cum_proportion, order_by = CONCOHORT)), 0, lag(cum_proportion, order_by = CONCOHORT)))

proportions_plot <- ggplot(data = diff_response_var_summary) +
  geom_bar(aes(x = CONCOHORT, y = cum_proportion), 
           stat = "identity", alpha = 0.1, width = 0.5) +
  geom_segment(data = diff_response_var_summary, 
               aes(x = CONCOHORT + 0.28, 
                   xend = CONCOHORT + 0.28, 
                   y = last_cum_proportion, 
                   yend = cum_proportion, 
                   col = "Discrete Likelihood"), 
               color = "darkred", lwd = 5) +
  
  geom_point(aes(x = CONCOHORT, y = cum_proportion), size = 3) +
  geom_line(aes(x = CONCOHORT, y = cum_proportion), size = 1) +
  
  theme_classic() +
  scale_y_continuous(name = "Cumulative Proportion (%)", 
                     sec.axis = sec_axis(~., name = "Proportion by Cohort (%)")) +
  labs(x = "Cohort")

ordinal_data_description <- grid.arrange(ordinal_frequency_proportion_plot, proportions_plot, log_cum_odds_plot, nrow = 1, widths = c(3, 2, 1.5))

ggsave("figures/modeling/ordinal-data-description.png", 
       plot = ordinal_data_description,
       width = 15, height = 5, dpi = 1000)
