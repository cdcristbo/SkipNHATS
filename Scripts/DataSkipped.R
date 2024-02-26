# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)

# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/"

# Read the necessary Excel files
Part2 <- read_excel(paste0(base_path, "SkipNHATS/datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "ItemResponse")
fullList <- read_excel(paste0(base_path, "SkipNHATS/datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "Item")
trueNames <- read_excel(paste0(base_path, "SkipNHATS/datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx"))



processData <- function(Part2, fullList, trueNames) {
  # Extract relevant columns from 'fullList'
  complete <- fullList %>% 
    select("tblItem-Category_Id", "tblItem-ItemTag", "tblItem-Numb",
           "tblQuestion-FieldRefusedSkiptoID", "tblQuestion-FieldDKSkiptoID",
           "tblQuestionText-QuestionText - EN", "tblQuestionText-QuestionText - ES") %>% 
    rename(fldSectionID = `tblItem-Category_Id`,
           fldItemID = `tblItem-ItemTag`)
  
  # Filter and join relevant data from 'complete', 'Part2', and 'trueNames'
  items <- complete %>% 
    filter(fldSectionID != "IS") %>% 
    left_join(Part2) %>% 
    filter(fldSectionID == "HC") %>% 
    left_join(trueNames %>% 
                rename(fldItemID = `Questionnaire ITEM`) %>% 
                distinct()) %>% 
    select(-c("tblQuestion-FieldRefusedSkiptoID", "tblQuestion-FieldDKSkiptoID"))
  
  # Rest of the code...
  
  # Return the final result
  return(FinalPresent)
}

# Call the function with the provided parameters

