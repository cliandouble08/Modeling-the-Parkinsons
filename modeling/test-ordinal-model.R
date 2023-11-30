source("package_prep.R")

test_ordinal_model_background <- ulam(
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
  chains = 7, 
  cores = 24, log_lik = TRUE
)

precis(test_ordinal_model_background)