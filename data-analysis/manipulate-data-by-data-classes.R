# # numeric_df_selected -----
# # Set aside PATNO column
# cols_to_adjust <- setdiff(names(numeric_df_selected), "PATNO")
# 
# updated_numeric_df_selected <- numeric_df_selected
# 
# # Change all columns to numeric class
# updated_numeric_df_selected[cols_to_adjust] <- lapply(updated_numeric_df_selected[cols_to_adjust], 
#                                                        function(x) as.numeric(as.character(x)))

# binomial_df_selected -----
## Some binomial variables have levels of 1 and 2, while most columns have 0 and 1
## Center all binomial variables to 0

# Set aside PATNO column
cols_to_adjust <- setdiff(names(binomial_df_selected), "PATNO")

updated_binomial_selected_df <- binomial_df_selected

# Change all columns to numeric class
updated_binomial_selected_df[cols_to_adjust] <- lapply(updated_binomial_selected_df[cols_to_adjust], 
                                                       function(x) as.numeric(as.character(x)))

# Adjust binomial levels to all 0 and 1
updated_binomial_selected_df[cols_to_adjust] <- lapply(updated_binomial_selected_df[cols_to_adjust], function(x) if(min(x, na.rm = TRUE) == 0) x + 1 else x)

# ordered_df_selected -----
## Some ordered categorical variables starts at level 1, reset it so it centers at 0

# Set aside PATNO column
cols_to_adjust <- setdiff(names(ordered_df_selected), "PATNO")

updated_ordered_df_selected <- ordered_df_selected

# Change all columns to numeric class
updated_ordered_df_selected[cols_to_adjust] <- lapply(updated_ordered_df_selected[cols_to_adjust], 
                                                       function(x) as.numeric(as.character(x)))

# Adjust binomial levels to all 0 and 1
updated_ordered_df_selected[cols_to_adjust] <- lapply(updated_ordered_df_selected[cols_to_adjust], function(x) if(min(x, na.rm = TRUE) == 0) x + 1 else x)

# qualitative_df_selected -----
## Some columns were not converted into dummy variables
## Center all columns to 0 after converting to dummy variables

# Set aside PATNO column
cols_to_adjust <- setdiff(names(qualitative_df_selected), "PATNO")

updated_qualitative_df_selected <- qualitative_df_selected

# Convert columns to factor dummy variables
updated_qualitative_df_selected[cols_to_adjust] <- lapply(updated_qualitative_df_selected[cols_to_adjust], function(x) as.integer(as.factor(x)))

# Change all columns to numeric class
updated_qualitative_df_selected[cols_to_adjust] <- lapply(updated_qualitative_df_selected[cols_to_adjust], 
                                                      function(x) as.numeric(as.character(x)))

# Adjust binomial levels to all 0 and 1
updated_qualitative_df_selected[cols_to_adjust] <- lapply(updated_qualitative_df_selected[cols_to_adjust], function(x) if(min(x, na.rm = TRUE) == 0) x + 1 else x)

# fundamental_df_selected -----
## Some columns were not converted into dummy variables
## Center all columns to 0 after converting to dummy variables

# Set aside PATNO column
cols_to_adjust <- setdiff(names(fundamental_df_selected), "PATNO")

updated_fundamental_df_selected <- fundamental_df_selected

# Convert columns to factor dummy variables
updated_fundamental_df_selected[cols_to_adjust] <- lapply(updated_fundamental_df_selected[cols_to_adjust], function(x) as.integer(as.factor(x)))

# Change all columns to numeric class
updated_fundamental_df_selected[cols_to_adjust] <- lapply(updated_fundamental_df_selected[cols_to_adjust], 
                                                          function(x) as.numeric(as.character(x)))

# Adjust binomial levels to all 0 and 1
updated_fundamental_df_selected[cols_to_adjust] <- lapply(updated_fundamental_df_selected[cols_to_adjust], function(x) if(min(x, na.rm = TRUE) == 1) x - 1 else x)

# Create a stand-alone response variable dataframe
## Only columns: PATNO, CONCOHORT
## Remove rows with CONCOHORT NA
response_var_df <- updated_fundamental_df_selected %>% 
  select(PATNO, CONCOHORT) %>% 
  filter(!is.na(CONCOHORT))