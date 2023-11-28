# Import data files
raw_df <- read_csv("data/ppmi_curated_data.csv")
raw_dictionary <- read_csv("data/ppmi_curated_data_dictionary.csv")

# Change CONCOHORT classification dummies

## Healthy control - 1
## Prodromal - 2
## PD - 3
raw_df <- raw_df %>% 
  mutate(CONCOHORT = case_when(
    CONCOHORT == 2 ~ 1,
    CONCOHORT == 4 ~ 2,
    CONCOHORT == 1 ~ 3,
    TRUE ~ CONCOHORT
  ), 
  COHORT = case_when(
    COHORT == 2 ~ 1,
    COHORT == 4 ~ 2,
    COHORT == 1 ~ 3,
    TRUE ~ COHORT
  ))

# Replace missing values as NA
## Function to replace "." with NA appropriately
replace_dot_with_NA <- function(column) {
  if (is.factor(column)) {
    levels(column) <- replace(levels(column), levels(column) == ".", NA)
    return(column)
  } else {
    return(ifelse(column == ".", NA, column))
  }
}

## Apply the function to each column of the dataframe
raw_df_na_updated <- raw_df %>% 
  mutate(across(everything(), replace_dot_with_NA))

# Exclude rows with CONCOHORT == NA
concohort_raw_df <- raw_df_na_updated %>% 
  filter(!is.na(CONCOHORT))

# Select rows with EVENT_ID == "BL" (first visit)
Year0_raw_df <- concohort_raw_df %>% 
  subset(EVENT_ID == "BL")

# Understand NA cases in the data
naCount_raw_df <- Year0_raw_df %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(cols = everything(), 
               names_to = "column_name", 
               values_to = "na_count") %>% 
  mutate(column_number = match(column_name, names(raw_df)), 
         na_proportion = na_count / nrow(raw_df))

# Confidence interval for na_proportion = (0.0169, 0.0847)
raw_ci <- t.test(naCount_raw_df$na_proportion)$conf.int
raw_ci_upper_limit <- raw_ci[2]

# Excluding all columns with na_proportion larger than 0.0847
updated_column_list <- naCount_raw_df %>% 
  subset(na_proportion <= 0.10)

removed_column_list <- naCount_raw_df %>% 
  subset(na_proportion > 0.10)

# Select filtered columns from raw_df
df <- Year0_raw_df %>% 
  select(all_of(updated_column_list$column_name))
# df: Data of the first visit for all patients (columns with na_proportion >= 0.10 were excluded)