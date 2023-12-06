df_selected <- df %>%  
  subset(select = -c(
    # Basic info
    SITE, EVENT_ID, YEAR, visit_date, age, 
    # History
    educ, fampd_bin, 
    # Biologic examinations
    abeta_LLOD, abeta_ULOD, ptau_LLOD, tau_LLOD,
    # Age at biological examinations
    agediag, ageonset, age_LP, age_DATSCAN, age_upsit, 
    # Data that are not shown for a certain group of patients
    CSFSAA, con_caudate, con_putamen, con_striatum, DATSCAN_CAUDATE_L, DATSCAN_CAUDATE_R, DATSCAN_PUTAMEN_L, DATSCAN_PUTAMEN_R, ips_caudate, ips_putamen, ips_striatum, lowput_ratio, mean_caudate, mean_putamen, mean_striatum, 
    # Data that are specifically for PD patients (i.e., medications)
    LEDD, 
    # Scores that only include ON and treated scores
    hy_on, NHY_ON, pigd_on, td_pigd_on, updrs_totscore_on, 
    # Examination scores that have been summed up into a total score
    updrs1_score, updrs2_score, updrs3_score, updrs3_score_on, 
    # Factor-adjusted score percentile
    upsit_pctl, upsit_pctl15, 
    # Others
    PRIMDIAG, duration, duration_yrs, DOMSIDE)) %>% 
  mutate(age_at_visit = as.numeric(age_at_visit))

# A new data dictionary for df_selected was created as imported
# Variables are classified by their properties as listed in Data Class column
data_class_dictionary <- read_csv("data/ppmi_curated_data_dictionary_classified.csv")

# Because of the way the original data dictionary was structured
# Rows with Variable == NA are removed
data_class_dictionary <- data_class_dictionary[!is.na(data_class_dictionary$Variable), ]

# Remove variables that were removed due to extensive proportion of missing values
vars_large_na_proportion <- removed_column_list$column_name
data_class_dictionary <- data_class_dictionary[!data_class_dictionary$Variable %in% vars_large_na_proportion, ]

# Remove some other variables
data_class_dictionary <- data_class_dictionary[data_class_dictionary$Variable != "SITE", ]

# Modify the table a little bit more 
data_class_dictionary$`Data Class`[data_class_dictionary$Variable == "MCI_testscores"] <- "numeric"
write_csv(data_class_dictionary, "data/ppmi_curated_data_dictionary_classified.csv")

# Select portions of df_selected based on variable's data class
numeric_vars <- data_class_dictionary$Variable[data_class_dictionary$`Data Class` %in% c('numeric', 'quantitative')]
numeric_vars <- unique(c("PATNO", numeric_vars))
binomial_vars <- data_class_dictionary$Variable[data_class_dictionary$`Data Class` == "binomial"]
binomial_vars <- unique(c("PATNO", binomial_vars))
ordered_vars <- data_class_dictionary$Variable[data_class_dictionary$`Data Class` == "ordered"]
ordered_vars <- unique(c("PATNO", ordered_vars))
qualitative_vars <- data_class_dictionary$Variable[data_class_dictionary$`Data Class` == "categorical"]
qualitative_vars <- unique(c("PATNO", qualitative_vars))
fundamental_vars <- data_class_dictionary$Variable[data_class_dictionary$`Data Class` == "fundamental"]
fundamental_vars <- unique(c("PATNO", fundamental_vars))
fundamental_vars <- unique(c("age_at_visit", fundamental_vars))

numeric_df_selected <- df_selected[, numeric_vars, drop = FALSE]
# Remove leading illegal characters from the variable name
colnames(numeric_df_selected) <- gsub("^_", "", colnames(numeric_df_selected))
colnames(numeric_df_selected) <- gsub("^[0-9]", "X\\0", colnames(numeric_df_selected))

binomial_df_selected <- df_selected[, binomial_vars, drop = FALSE]
ordered_df_selected <- df_selected[, ordered_vars, drop = FALSE]
qualitative_df_selected <- df_selected[, qualitative_vars, drop = FALSE]
fundamental_df_selected <- df_selected[, fundamental_vars, drop = FALSE]
fundamental_df_selected$age_at_visit <- standardize(fundamental_df_selected$age_at_visit)

# Include binomal, ordered, and qualitative varaibles into all_qualitative_df_selected
all_qualitative_df_selected <- merge(binomial_df_selected, ordered_df_selected, by = "PATNO")
all_qualitative_df_selected <- merge(all_qualitative_df_selected, qualitative_df_selected, by = "PATNO")