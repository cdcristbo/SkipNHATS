library(readr)
library(dplyr)
library(openxlsx)
library(readxl)
library(openxlsx)
library(tidyverse)


# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/SkipNHATS/"
fullList <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "Item")
trueNames <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx"))
Part2 <- read_excel(paste0(base_path, "datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "ItemResponse")
load(paste0(base_path, "outcomes/Rdresid.RData"))# result_df
load(paste0(base_path, "outcomes/SkipPattern.RData"))#results_df
processData <- paste0(base_path, "Functions/processData.R")
source(processData)
patternData = processData(Part2, fullList, trueNames)

trueNames2 <- result_df %>% 
mutate(label = paste0("hc1",label)) %>%
              rename('Variable name' = label) %>%
              left_join(trueNames %>% 
                          select(`Variable name`,`Questionnaire ITEM`)) %>%
                          select(`Variable name`,`Questionnaire ITEM`,indicatorByResIDValue,indicatorByResID) 


box = fullList %>% 
  distinct(`tblItem-ItemTag`,`tblQuestionText-QuestionText - EN`) %>% 
  filter(str_detect(`tblItem-ItemTag`, "BOX"))

a = load(paste0(base_path, "outcomes/FinalPresent.RData"))
b = load(paste0(base_path, "outcomes/combined_results.RData"))

combined_resultsHC_round1 = load(paste0(base_path, "outcomes/combined_results.Rdata"))
#combined_resultsHC_round1 <- read_csv("combined_resultsHC_round1.csv")
#save(results_df,file = "results_df.RData")
#load("results_df.RData")

datsRdresid = result_df %>% 
  mutate(firstskipPattern = paste0("hc1",label)) %>% 
  select(-label)

#merge both functions
# patternData = FinalPresent %>% 
# select(-name_if)  %>% 
# distinct()

patternData2 = patternData %>% 
  filter(!is.na(pattern)&!is.na(Variable.name))


datsRdresid = result_df %>% 
  mutate(firstskipPattern = paste0("hc1",label)) %>% 
  select(-label)


FinalPresentHC = patternData %>% 
  left_join(results_df %>% 
              rename(Variable.name = variable))  %>% 
  #select(-c(priorvariable,variable)) %>%
  left_join(result_df %>% 
              mutate(firstskipPattern = paste0("hc1",label)) %>% 
              select(-label) %>% 
              rename(Variable.name = firstskipPattern)) %>% 
  mutate(skipPrior = gsub(",\\s*-1", "", skipPrior)) %>%
  
  mutate(skippedResid = ifelse(OtherSkip2 == "1, 2",1,0),
         skippedbyBoth = ifelse(
    is.na(skipPrior) | 
      grepl("-8|-7|=(\\d+)", skipPrior) | 
      grepl("=(\\d+)", pattern),
    0,
    1
  ),
  skippedbyBoth = ifelse(is.na(pattern),2,skippedbyBoth) ) %>%
  left_join(box %>% 
              rename(boxText = `tblQuestionText-QuestionText - EN`,
                     Questionnaire.ITEM = `tblItem-ItemTag`)) %>% 
  # select(-c(id,nameSkip,countpat,name1,name2,fldSectionID,`tblItem-Numb`,
  #           `tblQuestionText-QuestionText - EN`,`tblQuestionText-QuestionText - ES`,
  #           fldResponseSchemeName,group_index,`Variable name`,`Variable label`,`1.x`)) %>% 
  # rename(Resid1Inaplicable= `1.y`) %>%
 # select(-name_if) %>%
 # distinct()  %>%
  #mutate(indicatorResid = ifelse(Resid1Inaplicable=="1, 2",1,0)   ) %>% 
  #mutate(  Indicator2 =ifelse(is.na(Indicator2),0,Indicator2)   ) %>%
  mutate(Indicator2 = case_when(skippedResid==1 & skippedbyBoth==0 ~ 0,
                                skippedResid==1 & skippedbyBoth==1 ~ 1,
                                skippedResid==1 & skippedbyBoth==2 ~ 1,
                                skippedResid==0 & skippedbyBoth==1 ~ 1,
                                skippedResid==0 & skippedbyBoth==2 ~ 0,  
                                skippedResid==0 & skippedbyBoth==0 ~ 0)  ) %>%
# until here works
    left_join(results_box %>% 
              rename(Questionnaire.ITEM = variable)) %>% 
  mutate(IndicatorBox = ifelse(boxVal==-1 | 
                                 boxValminus %in% c(-8, -7)   |
                                 tail(strsplit(as.character(case), ",")[[1]], 1) == boxValminus,0,1)) %>% 
  mutate(IndicatorBox = ifelse(is.na(IndicatorBox),0,IndicatorBox)) %>% 
  select(-case) %>% 
    select(Questionnaire.ITEM, pattern,skipPrior,Resid1Inaplicable,boxText,boxValminus, starts_with("Indicator")) %>% 
  mutate(just = ifelse(!is.na(skipPrior),paste0(pattern,skipPrior),0  ),
         just2 = ifelse(Resid1Inaplicable == "1, 2",paste0("resid=",Resid1Inaplicable),0  )) %>% 
  mutate(finalJust = ifelse(Indicator2==1,paste0(just,just2),0)) %>% 
  mutate(finalJusttexts = paste0(just," or ",just2)) %>% 
  mutate(finalJusttexts = ifelse(finalJusttexts=="0 or 0"|finalJusttexts=="0 or NA" ,
                                 NA,finalJusttexts
                                 )) %>%
                                 left_join(trueNames2 %>% 
                                 mutate(texResid = "resid=4") %>%
                                             rename(Questionnaire.ITEM =`Questionnaire ITEM`))  %>%
                                             select(-c(just2,finalJust,finalJusttexts))

View(FinalPresentHC)

#write.csv(FinalPresentHC, file = paste0(base_path, "outcomes/delete18.csv"), append = FALSE, quote = TRUE, sep = " ")
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
