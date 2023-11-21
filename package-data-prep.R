# Import Packages
libraries <- c("rethinking", "ggplot2", "dplyr", "readr", "lubridate", "tidyr")

new_packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Import data files
raw_df <- read_csv("D:/OneDrive - Grinnell College/Group Project- Parkinson's Disease/000 Datasets/999 Curated Data Cut/PPMI_Curated_Data_Cut_Public_20230612.csv")

# Understand NA cases in the data
naCount_raw_df <- raw_df %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(cols = everything(), 
               names_to = "column_name", 
               values_to = "na_count") %>% 
  mutate(column_number = match(column_name, names(raw_df)), 
         na_proportion = na_count / nrow(raw_df))

