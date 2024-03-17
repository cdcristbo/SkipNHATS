# Load necessary libraries
library(dplyr)  # For data manipulation
library(haven)  # For reading data files
library(tidyr)  # For data tidying
library(readxl)  # For reading Excel files
library(sas7bdat)  # For reading SAS files
library(stringr)  # For string manipulation

# Specify the path to your Excel file
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/SkipNHATS/"

# Read and clean the Crosswalk data
Crosswalk <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx")) %>% 
  distinct(`Questionnaire ITEM`, `Variable name`)

# Set the path to the R script
getRowsWithNonzeroValues <- paste0(base_path, "Functions/getRowsWithNonzeroValues.R")

# Set the folder path
folder_path <- paste0(base_path, "datasets/SP")
file_listSen = paste0(base_path,"datasets/sensitiveSP/r1/NHATS_Round_1_SP_Sen_Dem_File.sas7bdat")
# Load the R script containing the getRowsWithNonzeroValues function
source(getRowsWithNonzeroValues)

# Get a list of all .dta files in the folder
file_list <- list.files(path = folder_path, pattern = ".dta", full.names = TRUE)
file_list <- file_list[1]  # Select the first file only

# Create an empty dataframe to store the results
result_df <- data.frame(variable = character(0), round1Inaplicable = character(0))

# Iterate through each dataset file
for (file in file_list) {
  # Read the dataset
  data <- read_dta(file)
  
  # Read the sensitive data
  dataSen <- read.sas7bdat(file_listSen)
  
  # Merge the dataset with the sensitive data
  data <- data %>% 
    left_join(dataSen)
  
  # Extract relevant columns from the dataset
  HCAll <- data %>% 
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
result_df <- result_df %>%  # Assign the modified dataframe back to result_df
  select(-c(re, OtherSkip)) %>%  # Remove the columns 're' and 'OtherSkip' from the dataframe
  mutate(indicatorByResID = ifelse(indicatorByResIDValue == "-1", 1, 0)) %>%  # Create a new column 'indicatorByResID' with values 1 if 'indicatorByResIDValue' is "-1", otherwise 0
  mutate(round = as.integer(gsub("hc(\\d+).*", "\\1", variable))) %>%  # Extract the numeric part from the 'variable' column and assign it to the new column 'round'
  mutate(label = sub("hc\\d+", "", variable)) %>%  # Remove the 'hc' prefix and the numeric part from the 'variable' column and assign it to the new column 'label'
  select(-variable) %>%  # Remove the 'variable' column from the dataframe
  filter(round == 1)  # Keep only the rows where 'round' is equal to 1

save(result_df, file = paste0(base_path, "outcomes/Rdresid.RData"))
