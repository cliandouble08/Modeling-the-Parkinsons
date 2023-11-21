# Import Packages
libraries <- c("rethinking", "ggplot2", "dplyr", "readr", "lubridate", "tidyr", 
               "mosaic")

new_packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Import data files
raw_df <- read_csv("data/ppmi_curated_data.csv")
raw_dictionary <- read_csv("data/ppmi_curated_data_dictionary.csv")

# Exclude rows with CONCOHORT == NA
concohort_raw_df <- raw_df %>% 
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