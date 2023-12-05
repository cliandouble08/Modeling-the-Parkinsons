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
      b_scopa * scopa + b_SDMTOTAL * SDMTOTAL + 
      b_stai_state * stai_state + b_updrs_totscore * updrs_totscore +
      
      # Categorical variables
      b_td_pigd[td_pigd] + b_NHY[NHY] + b_NP1FATG[NP1FATG], 
    
    c(b_scopa, b_SDMTOTAL, b_stai_state, b_updrs_totscore) ~ dnorm(0, 3),
    b_td_pigd[td_pigd] ~ dnorm(0, 3), 
    b_NHY[NHY] ~ dnorm(0, 3),
    b_NP1FATG[NP1FATG] ~ dnorm(0, 3),
    
    cutpoints ~ dnorm(0, 1.5)
  ), data = model_df_na_free, 
  chains = 24, warmup = 3000, iter = 15000, 
  cores = 24, log_lik = TRUE
)

ordinal_precis <- precis(ordinal_model, depth = 2)
cutpoints_inv_logit <- format(inv_logit(coef(ordinal_model)), digits = 3)

ordinal_posterior <- extract.samples(ordinal_model)

# Store traceplots into multiple files
source("modeling/store-traceplots.R")

test_ordinal_model <- ulam(
  alist(
    CONCOHORT ~ dordlogit(phi, cutpoints),
    phi <-
      # Numerical variables
      b_scopa * scopa + b_SDMTOTAL * SDMTOTAL + b_stai_state * stai_state + b_updrs_totscore * updrs_totscore +
      # Categorical variables
      b_td_pigd[td_pigd] + b_NHY[NHY] + b_NP1FATG[NP1FATG],
    
    c(b_scopa, b_SDMTOTAL, b_stai_state, b_updrs_totscore) ~ dnorm(0, 2),
    b_td_pigd[td_pigd] ~ dnorm(0, 2), 
    b_NHY[NHY] ~ dnorm(0, 2),
    b_NP1FATG[NP1FATG] ~ dnorm(0, 2),
    
    cutpoints ~ dnorm(0, 1.5)
  ), data = model_df_na_free, 
  chains = 20, 
  cores = 24, log_lik = TRUE
)

## Missing value-imputed model -----
# For missing values: Statistical Rethinking, Chapter 15.2.2
# Each missing value is assigned with a parameter
ordinal_model <- ulam(
  alist(
    CONCOHORT ~ dordlogit(mu, cutpoints), 
    mu <- a + b1 * B + b2 * C, 
    
    # Impute missing values by correlation between B and C
    MB ~ multi_normal( c(muc,mub) , Rho_Bc , Sigma_Bc),
    matrix[29,2]:BC <<- append_col(C , B),
    # Define B1 as mix of observed and imputed values
    vector[29]:B <- merge_missing(B, B_impute), 
    
    c(a, muc, mub) ~ dnorm(0, 1.5), 
    c(b1, b2) ~ dnorm(0, 1.5),
    
    sigma ~ dexp(1)
    Rho_Bc ~ lkj_corr(2), 
    Sigma_Bc ~ dexp(1)
    
    cutpoints ~ dnorm(0, 1.5)
  ), data = model_df, 
  chains = 8, warmup = 5000, iter = 15000, 
  cores = 24, log_lik = TRUE, messages = FALSE
)

traceplot(ordinal_model)