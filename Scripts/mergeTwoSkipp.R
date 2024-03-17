# Load required libraries
library(readr)
library(dplyr)
library(openxlsx)
library(readxl)
library(openxlsx)
library(tidyverse)

# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/SkipNHATS/"

# Read data from Excel files
fullList <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "Item")
trueNames <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx"))
Part2 <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "ItemResponse")

# Load saved data
load(paste0(base_path, "outcomes/Rdresid.RData")) # result_df
load(paste0(base_path, "outcomes/SkipPattern.RData")) # results_df

# Load custom function
processData <- paste0(base_path, "Functions/processData.R")
source(processData)

# Process data using custom function
patternData <- processData(Part2, fullList, trueNames)

# Remove unnecessary columns from result_df
datsRdresid <- result_df %>% 
  mutate(firstskipPattern = paste0("hc1", label)) %>% 
  select(-label)

# Filter out rows with missing values in pattern and Variable.name columns
patternData2 <- patternData %>% 
  filter(!is.na(pattern) & !is.na(Variable.name))

# Remove unnecessary columns from result_df
datsRdresid <- result_df %>% 
  mutate(firstskipPattern = paste0("hc1", label)) %>% 
  select(-label)

# Merge patternData, results_df, and result_df
FinalPresentHC <- patternData %>% 
  left_join(results_df %>% 
              rename(Variable.name = variable)) %>% 
  left_join(result_df %>% 
              mutate(firstskipPattern = paste0("hc1", label)) %>% 
              select(-label) %>% 
              rename(Variable.name = firstskipPattern)) %>% 
  mutate(skipPrior = gsub(",\\s*-1", "", skipPrior)) %>%
  mutate(skippedResid = ifelse(OtherSkip2 == "1, 2", 1, 0),
         skippedbyBoth = ifelse(
           is.na(skipPrior) | 
           grepl("-8|-7|=(\\d+)", skipPrior) | 
           grepl("=(\\d+)", pattern),
           0,
           1
         ),
         skippedbyBoth = ifelse(is.na(pattern), 2, skippedbyBoth)) %>%
  mutate(Indicator2 = case_when(
    skippedResid == 1 & skippedbyBoth == 0 ~ 0,
    skippedResid == 1 & skippedbyBoth == 1 ~ 1,
    skippedResid == 1 & skippedbyBoth == 2 ~ 1,
    skippedResid == 0 & skippedbyBoth == 1 ~ 1,
    skippedResid == 0 & skippedbyBoth == 2 ~ 0,  
    skippedResid == 0 & skippedbyBoth == 0 ~ 0
  )) %>%
  select(c("fldSectionID", "Questionnaire.ITEM", "Variable.name", "fldResponseID", "pattern", "OtherSkip2",
           "indicatorByResIDValue", "skipPrior", "indicatorByResID", "minus1Target",
           "skippedResid", "skippedbyBoth", "Indicator2")) %>% 
  mutate(pattern = str_replace(pattern, "=\\d+$", ""),
         pattern = ifelse(!is.na(skipPrior), paste0(pattern, "=", skipPrior), NA),
         textResID = ifelse(!is.na(indicatorByResID), "resid=4", NA),
         text = case_when(
           is.na(textResID) & is.na(pattern) ~ NA,
           is.na(textResID) & !is.na(pattern) ~ pattern,
           !is.na(textResID) & is.na(pattern) ~ textResID,
           !is.na(textResID) & !is.na(pattern) ~ paste0(textResID, " or ", pattern)
         ),
         text = ifelse(is.na(text), "FileNotinSP", text)) %>% 
  select(-c(textResID, pattern)) %>% 
  ungroup() %>% 
  group_by(Questionnaire.ITEM) %>% 
  distinct() %>% 
  mutate(fldResponsesID = paste(fldResponseID, collapse = ",")) %>% 
  ungroup() %>% 
  select(-fldResponseID) %>% 
  distinct() %>% 
  select("fldSectionID", "Questionnaire.ITEM", "Variable.name", "fldResponsesID", "OtherSkip2", "indicatorByResIDValue", "skipPrior", "text") %>% 
  mutate(skipbyuniplicable = case_when(
    is.na(OtherSkip2) ~ NA,
    OtherSkip2 == " " ~ 0,
    !is.na(OtherSkip2) ~ 1
  ),
  skipbyResIDValue = case_when(
    indicatorByResIDValue == "-1" ~ 1,
    is.na(indicatorByResIDValue) ~ NA,
    indicatorByResIDValue != "-1" ~ 0
  ),
  skipbyResIDPattern = case_when(
    is.na(skipbyuniplicable) & is.na(skipPrior) ~ NA,
    skipbyuniplicable == 0 & !is.na(skipPrior) ~ 0,
    skipbyuniplicable == 0 & is.na(skipPrior) ~ 0,
    skipbyuniplicable == 1 & !is.na(skipPrior) ~ 0,
    skipbyuniplicable == 1 & is.na(skipPrior) ~ 1
  )) %>% 
  select(-c("OtherSkip2", "indicatorByResIDValue", "skipPrior")) %>% 
  select("fldSectionID", "Questionnaire.ITEM", "Variable.name", "fldResponsesID",    
         "skipbyResIDValue","skipbyuniplicable", "skipbyResIDPattern", "text") %>% 
  rename(AdditionalSkipByResID = "skipbyuniplicable",
         skipbyResID = "skipbyResIDValue",
         skipbyResIDPattern = "skipbyResIDPattern")


