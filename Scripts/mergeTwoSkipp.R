library(readr)
library(dplyr)
library(openxlsx)
library(readxl)
library(openxlsx)
library(tidyverse)


# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/SkipNHATS/"

box = fullList %>% 
  distinct(`tblItem-ItemTag`,`tblQuestionText-QuestionText - EN`) %>% 
  filter(str_detect(`tblItem-ItemTag`, "BOX"))

load(paste0(base_path, "outcomes/FinalPresent.RData"))
load(paste0(base_path, "outcomes/combined_results.RData"))

combined_resultsHC_round1 = load(paste0(base_path, "outcomes/combined_results.Rdata"))
#combined_resultsHC_round1 <- read_csv("combined_resultsHC_round1.csv")
#save(results_df,file = "results_df.RData")
#load("results_df.RData")
datsRdresid = final %>% 
  mutate(firstskipPattern = paste0("hc1",label)) %>% 
  select(-label)
#merge both functions
patternData = FinalPresent

patternData2 = patternData %>% 
  filter(!is.na(pattern)&!is.na(Variable.name))

FinalPresentHC = patternData %>% 
  left_join(results_df %>% 
              rename(Variable.name = variable))  %>% 
  #select(-c(priorvariable,variable)) %>%
  left_join(datsRdresid %>% 
              rename(Variable.name = firstskipPattern)) %>% 
  mutate(skipPrior = gsub(",\\s*-1", "", skipPrior)) %>%
  mutate(skippedResid = ifelse(is.na(`1.y`) | `1.y` == " ",0,1),
         skippedbyBoth = ifelse(
    is.na(skipPrior) | 
      grepl("-8|-7|=(\\d+)", skipPrior) | 
      grepl("=(\\d+)", pattern),
    0,
    1
  ),
  skippedbyBothFinal = case_when(skippedResid==1 & is.na(pattern) ~1,
                                 skippedResid==1 & !is.na(pattern)~2,
                                 skippedResid==0 & is.na(pattern)~3,
                                 skippedResid==0 & !is.na(pattern)~4)) %>%
  left_join(box %>% 
              rename(boxText = `tblQuestionText-QuestionText - EN`,
                     Questionnaire.ITEM = `tblItem-ItemTag`)) %>% 
  select(-c(id,nameSkip,countpat,name1,name2,fldSectionID,`tblItem-Numb`,
            `tblQuestionText-QuestionText - EN`,`tblQuestionText-QuestionText - ES`,
            fldResponseSchemeName,group_index,`Variable name`,`Variable label`,`1.x`)) %>% 
  rename(Resid1Inaplicable= `1.y`) %>%
  select(-name_if) %>%
  distinct() 
View(FinalPresentHC)

write.csv(FinalPresentHC, file = paste0(base_path, "outcomes/delete2.csv"), append = FALSE, quote = TRUE, sep = " ")
#write.csv(FinalPresentHC,file = "FinalPresentHCStudy.csv")
FinalPresentHC

boxExample = data %>% 
  distinct(spid,r1dresid,hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  #group_by(r1dresid,hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  group_by(hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  summarise(freq = n()) %>% 
  mutate(patternbox = ifelse(hc1fllsinmth==1&hc1faleninyr!= -1 ,1,0),
         skip = ifelse(sum(patternbox)>0,1,0))

View(boxExample)
data.frame(table(data$hc1fllsinmth,data$hc1faleninyr)) 

table(data$hc1dementage,data$hc1disescn9)

boxExample = data %>% 
  distinct(spid,r1dresid,hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  #group_by(r1dresid,hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  group_by(hc1fllsinmth,hc1faleninyr,hc1multifall) %>% 
  summarise(freq = n()) %>% 
  mutate(patternbox = ifelse(hc1fllsinmth==1&hc1faleninyr!= -1 ,1,0),
         skip = ifelse(sum(patternbox)>0,1,0))
