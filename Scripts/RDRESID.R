# Load necessary libraries
library(dplyr)
library(haven)
library(tidyr)
library(reshape2)
library(readxl)

# Read and clean the Crosswalk data
Crosswalk <- read_excel("NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx") %>% 
  distinct(`Questionnaire ITEM`, `Variable name`)

# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/workNhats/"

# Set the path to the R script
getRowsWithNonzeroValues <- paste0(base_path, "Functions/getRowsWithNonzeroValues.R")

# Set the folder path
folder_path <- paste0(base_path, "datasets/SP")

# Load the R script containing the getRowsWithNonzeroValues function
source(getRowsWithNonzeroValues)

# Get a list of all .dta files in the folder
file_list <- list.files(path = folder_path, pattern = ".dta", full.names = TRUE)
# file_list = file_list[1]

# Create an empty dataframe to store the results
result_df <- data.frame(variable = character(0), round1Inaplicable = character(0))

# Iterate through each dataset file
for (file in file_list) {
  # Read the dataset
  data <- read_dta(file_list)
  
  # Extract relevant columns from the dataset
  HCAll = data %>% 
    select(spid, ends_with("dresid"), starts_with("hc"))
  
  # Extract column names with the pattern "^r\\d+dresid$"
  resid_columns <- names(HCAll)[grepl("^r\\d+dresid$", names(HCAll))]
  
  # Find the column with the maximum value
  max_value_column <- resid_columns[which.max(sapply(HCAll[resid_columns], max))]
  
  # Select relevant columns from the dataset (HC)
  HC <- HCAll %>% 
    select(spid, max_value_column, starts_with("hc")) 
  
  # Get the names of the columns starting with "hc"
  hc_columns <- grep("^hc", names(HC), value = TRUE)
  
  # Iterate through each hc_column
  for (col in hc_columns) {
    tryCatch({
      # Call the function and append the results to the dataframe
      result_df <- rbind(result_df, getRowsWithNonzeroValues(HC, col))
    }, error = function(e) {
      cat(paste("Error occurred for HC and column:", col, "\n"))
    })
  }
}

# Process and clean the result dataframe
result_df2 <- result_df %>% 
  select(-c(re, OtherSkip)) %>% 
  mutate(round = as.integer(gsub("hc(\\d+).*", "\\1", variable))) %>% 
  mutate(label = sub("hc\\d+", "", variable)) %>%
  select(-variable) %>% 
  filter(round==1)

# Reshape the dataframe using spread function
spread_df <- spread(result_df2 %>% select(-OtherSkip2), key = round, value = round1Inaplicable)
spread_df2 <- spread(result_df2 %>% select(-round1Inaplicable), key = round, value = OtherSkip2)

# Combine the reshaped dataframes
final = spread_df %>% 
  left_join(spread_df2, by = "label")

# Define the desired column order
column_order <- c("label", "1.x", "1.y", "2.x", "2.y", "3.x", "3.y", "4.x", "4.y", "5.x", "5.y", "6.x", "6.y", 
                  "7.x", "7.y", "8.x", "8.y", "9.x", "9.y", "10.x","10.y","11.x","11.y", "12.x", "12.y")

# Reorder the columns in the 'final' data frame
final <- final %>%
  select(column_order)  

# Define the new column names
new_names <- c("label", 
               "1.Inaplicable", "1.OtherSkip", 
               "2.Inaplicable", "2.OtherSkip", 
               "3.Inaplicable", "3.OtherSkip", 
               "4.Inaplicable", "4.OtherSkip", 
               "5.Inaplicable", "5.OtherSkip", 
               "6.Inaplicable", "6.OtherSkip", 
               "7.Inaplicable", "7.OtherSkip", 
               "8.Inaplicable", "8.OtherSkip", 
               "9.Inaplicable", "9.OtherSkip", 
               "10.Inaplicable", "10.OtherSkip", 
               "11.Inaplicable", "11.OtherSkip", 
               "12.Inaplicable", "12.OtherSkip")

# Rename the columns
final2 <- final %>%
  rename_with(~ new_names, everything()) %>% 
  mutate_all(~na_if(., "")) 

# Save the combined results to a file if desired
# save(final, file = paste0(base_path, "outcomes/combined_results.RData"))

finalVer = final2 %>% 
  mutate(`Variable name` = paste0("hc1",label)) %>% 
  left_join(Crosswalk)

subset_result <- Crosswalk %>%
  filter(grepl("HC", `Questionnaire ITEM`, ignore.case = TRUE))

# setdiff(subset_result$`Questionnaire ITEM`,unique(finalVer$`Questionnaire ITEM`) )
# setdiff(unique(finalVer$`Questionnaire ITEM`),subset_result$`Questionnaire ITEM`)
