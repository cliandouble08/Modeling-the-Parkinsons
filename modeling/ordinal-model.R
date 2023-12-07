# Pre-load the following
load(".RData")
source("package_prep.R")

# model_df -----
# Categorical variable list: categorical_vars_selected
# Numerical variable list: selected_vars

## Complete-case model -----
model_df_updated <- model_df %>% 
  # Convert categorical columns to integers
  mutate(across(all_of(categorical_vars_selected), as.integer)) %>% 
  # Convert numerical columns to numeric
  mutate(across(all_of(selected_vars), as.numeric)) %>% 
  dplyr::select(-c(PDTRTMNT, ASHKJEW, hy, upsit)) %>% 
  mutate(CONCOHORT = as.integer(CONCOHORT + 1))
model_df_na_free <- na.omit(model_df_updated) 
  
# Complete-case ordinal model without linear regression
ordinal_model_plain <- ulam(
  alist(
    CONCOHORT ~ dordlogit(0, cutpoints),
    cutpoints ~ dnorm(0, 10)
  ), data = model_df_na_free, 
  chains = 24, warmup = 1.5e4, iter = 1e6, 
  cores = 24, log_lik = TRUE, messages = FALSE
)

# The posterior distribution of the cutpoints is on the log-cumulative-odds scale
precis(ordinal_model_plain, depth = 2)

# `inv_logit(coef(ordinal_model))` gives the cumulative-proportion for each response
plain_ordinal_cutpoints_inv_logit <- format(inv_logit(coef(ordinal_model_plain)), digits = 3)

# Extract samples
plain_ordinal_posterior <- extract.samples(ordinal_model_plain)

# Complete-case ordinal model with linear regression
ordinal_model <- ulam(
  alist(
    # Ordinal model
    CONCOHORT ~ dordlogit(phi, cutpoints),
    phi <-
      # Numerical variables
      b_age_at_visit * age_at_visit + b_scopa * scopa + b_SDMTOTAL * SDMTOTAL + 
      b_stai_state * stai_state + b_updrs_totscore * updrs_totscore +
      
      # Categorical variables
      b_td_pigd[td_pigd] + b_NHY[NHY] + b_NP1FATG[NP1FATG],
    
    c(b_age_at_visit, b_scopa, b_SDMTOTAL, 
      b_stai_state, b_updrs_totscore) ~ dnorm(0, 5),
    b_td_pigd[td_pigd] ~ dnorm(0, 5), 
    b_NHY[NHY] ~ dnorm(0, 5),
    b_NP1FATG[NP1FATG] ~ dnorm(0, 5),
    
    cutpoints ~ dnorm(0, 5)
  ), data = model_df_na_free, 
  chains = 20, warmup = 3000, iter = 15000, 
  cores = 24, log_lik = TRUE
)

ordinal_precis <- precis(ordinal_model, depth = 2)
cutpoints_inv_logit <- format(inv_logit(coef(ordinal_model)), digits = 3)

# Plot effect size for each explanatory variable
png(filename = "figures/modeling/posterior/posterior_precis.png", 
    width = 5, height = 5, units = "in", res = 1000)
plot(ordinal_precis)
dev.off()

# Plot cumulative proportion by cutpoints
cutpoints_posterior <- as.data.frame(ordinal_posterior[["cutpoints"]])

cutpoint_1 <- mean(cutpoints_posterior$V1)
cutpoint_2 <- mean(cutpoints_posterior$V2)

invlogit_cutpoint_1 <- inv_logit(cutpoint_1)
invlogit_cutpoint_2 <- inv_logit(cutpoint_2)

cumulative_posterior <- data.frame(
  level = c(0, 1, 2, 3),
  value = c(0, as.numeric(invlogit_cutpoint_1), as.numeric(invlogit_cutpoint_2), 1)
)

ggplot(data = cumulative_posterior, 
       mapping = aes(x = level, y = value)) +
  geom_point() +
  geom_text(aes(label = format(value, digits = 2)), vjust = -1.0, hjust = 0.5, size = 3) +
  geom_line() +
  
  theme_par() +
  labs(title = "Cumulative Proportion for Each Cohort", 
       x = "Cohort", 
       y = "Cumulative proportion") +
  ylim(0, 1.2)
ggsave(file = "figures/modeling/posterior/cumulative-proportions.png", 
       last_plot(), 
       width = 5, height = 5, dpi = 1000)
dev.off()
