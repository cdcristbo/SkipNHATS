# Load necessary libraries
library(dplyr)
library(haven)
library(readxl)
library(stringr)
# Specify the path to your Excel file

# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/SkipNHATS/"

# Read the necessary Excel files
Part2 <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "ItemResponse")
fullList <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "Item")
trueNames <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx"))

load(paste0(base_path, "outcomes/FinalPresent.RData"))
getRowsGroup1 <- paste0(base_path, "Functions/getRowsGroup1.R")
getRowsGroup2 <- paste0(base_path, "Functions/getRowsGroup2.R")
processData <- paste0(base_path, "Functions/processData.R")

# Load the R script containing getRowsGroup1 and getRowsGroup2 function2
source(getRowsGroup1)
source(getRowsGroup2)
source(processData)
# Set the folder path
folder_path <- paste0(base_path, "datasets/SP")

# Get a list of all .dta files in the folder
file_list <- list.files(path = folder_path, pattern = ".dta", full.names = TRUE)
file_list = file_list[1]
# Create an empty dataframe to store the results
result_df <- data.frame(variable = character(0), round1Inaplicable = character(0))
data <- read_dta(file_list)

#FinalPresent<- processData(Part2, fullList, trueNames)
#patternData = FinalPresent
patternData = processData(Part2, fullList, trueNames)

patternData = patternData %>% 
  mutate(pattern = ifelse(Variable.name==name1,NA,pattern)) %>% 
  filter(!is.na(pattern) & !is.na(Variable.name))

results_df = NULL
# Iterate over each row in patternData
for (i in seq_len(nrow(patternData))) {
  tryCatch({
    # Extract the targetColumn, skipVariable, and pattern from the current row
    targetColumn <- patternData[i, "Variable.name"]
    skipVariable <- patternData[i, "name1"]
    pattern <- patternData[i, "pattern"]
    
    # Check conditions to decide which function to apply
    if (!is.na(pattern) && !grepl(",", pattern)) {
      result <- getRowsGroup1(data, patternData[i, ], patternData[i, ]$Variable.name, patternData[i, ]$name1)
    } else if (!is.na(pattern) && grepl(",", pattern)) {
      result <- getRowsGroup2(data, patternData[i, ], patternData[i, ]$Variable.name, patternData[i, ]$name2)
    } else {
      result <- data.frame(
        variable = targetColumn,
        priorvariable = skipVariable,
        outcome = NA,
        stringsAsFactors = FALSE
      )
    }
    
    # Append the result to the dataframe
    results_df <- rbind(results_df, result)
    
  }, error = function(e) {
    cat("Error in row", i, ":", conditionMessage(e), "\n")
  })
}
results_df = results_df %>% 
  distinct()
View(results_df)
#write.csv(results_df, file = paste0(base_path, "outcomes/delete.csv"), append = FALSE, quote = TRUE, sep = " ")
#save(results_df, file = paste0(base_path, "outcomes/results_dfV02.RData"))