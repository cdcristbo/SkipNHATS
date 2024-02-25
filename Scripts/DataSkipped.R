# Load necessary libraries
library(readxl)
library(stringr)

# Set common base path
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/"

# Read the necessary Excel files
Part2 <- read_excel(paste0(base_path, "workNhats/datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "ItemResponse")

fullList <- read_excel(paste0(base_path, "workNhats/datasets/SkipDataset/NHATSNationalStudyRound1SpecWriterExchange.xlsx"), sheet = "Item")

trueNames <- read_excel(paste0(base_path, "workNhats/datasets/SkipDataset/NHATS_R1_Crosswalk_between_Instruments_and_Codebook_0.xlsx"))

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
  group_by(fldItemID) %>%
  mutate(group_index = cur_group_id()) %>% 
  ungroup() %>%
  filter(fldSectionID == "HC") %>% 
  #filter(fldItemID != "HC3") %>% 
  left_join(trueNames %>% 
              rename(fldItemID = `Questionnaire ITEM`) %>% 
              distinct()) %>% 
  select(-c("tblQuestion-FieldRefusedSkiptoID", "tblQuestion-FieldDKSkiptoID"))

# Inapplicable part
itemsHC <- items %>% 
  rename(`Questionnaire ITEM` = fldItemID,
         id = `tblItem-Numb`) %>% 
  arrange(id) %>% 
  filter(str_detect(`Questionnaire ITEM`, "HC")) %>% 
  #filter(`Questionnaire ITEM` != "HC3") %>% 
  select(c("Questionnaire ITEM", "id", "fldResponseID", "fldSkipTo", "Variable name"))

# Extract unique patterns and positions
pattern <- unique(itemsHC$fldSkipTo)
positions <- sapply(pattern, function(pat) {
  which(itemsHC$`Questionnaire ITEM` == pat)[1]
})

# Create data frames for patterns and positions
paste_df <- data.frame(cbind(pattern, position = as.numeric(positions)))[-1,]
paste2_df <- itemsHC %>% 
  filter(!is.na(fldSkipTo)) %>% 
  distinct(`Questionnaire ITEM`, fldSkipTo) %>% 
  group_by(fldSkipTo) %>% 
  mutate(del = seq(n())) %>% 
  filter(del == 1) %>% 
  ungroup() %>% 
  select(-del) %>% 
  rename(patt = `Questionnaire ITEM`) %>% 
  rename(`Questionnaire ITEM` = fldSkipTo) %>% 
  right_join(itemsHC) %>% 
  arrange(id) %>% 
  group_by(patt) %>% 
  mutate(countpat = seq_along(patt)) %>% 
  ungroup() %>% 
  mutate(pattdel = case_when(is.na(patt) ~ 0,
                             !is.na(patt) & countpat == 1 ~ 1,
                             !is.na(patt) & countpat != 1 ~ 0)) %>% 
  mutate(patt = ifelse(pattdel == 1, patt, NA)) %>% 
  select(-pattdel) %>% 
  mutate(patt = lead(patt)) %>% 
  mutate(patt2 = ifelse(!is.na(fldSkipTo), `Questionnaire ITEM`, patt)) %>% 
  mutate(patt2 = ifelse(grepl("HC", patt2), patt2, NA)) %>% 
  mutate(crossed_column = ifelse(!is.na(patt), as.character(patt), as.character(patt2))) %>% 
  select(-c(patt, patt2))

# Merge data frames for IDs
idMerge <- paste2_df %>% 
  distinct(`Questionnaire ITEM`, id) %>% 
  rename(id2 = id,
         crossed_column = `Questionnaire ITEM`)

# Merge data frames for final results
fin2 <- paste2_df %>% 
  left_join(idMerge)

# Process the grouped positions
df <- data.frame(position = seq_along(fin2$id2), value = fin2$id2)
df_filtered <- df %>% filter(!is.na(value))

# Group the positions by their values
grouped_positions <- df_filtered %>% 
  group_by(value) %>% 
  mutate(n = n(),
         min = min(position) + 1,
         max = max(position) - 1) %>% 
  ungroup() %>% 
  filter(n != 1) %>% 
  distinct(value, min, max)

grouped_positions2 <- df_filtered %>% 
  group_by(value) %>% 
  mutate(n = n(),
         min = min(position) + 1,
         max = max(position) - 1) %>% 
  ungroup() %>% 
  mutate(id3 = lead(max)) %>% 
  select(-max) %>% 
  rename(max = id3) %>% 
  filter(n == 1) %>% 
  distinct(value, n, min, max)

# Fill in values for grouped_positions and grouped_positions2
id1 <- rep(NA, nrow(fin2))
for (i in 1:nrow(grouped_positions)) {
  id1[grouped_positions$min[i]:grouped_positions$max[i]] <- grouped_positions$value[i]
}

fin3 <- cbind(fin2, id1)

id2 <- rep(NA, nrow(fin3))
for (i in 1:nrow(grouped_positions2)) {
  id2[grouped_positions2$min[i]:grouped_positions2$max[i]] <- grouped_positions2$value[i]
}

fin4 <- data.frame(cbind(fin3, id2))

# Create 'text' column
fin4$text <- paste(fin4$Questionnaire.ITEM, fin4$fldResponseID, sep = "=")

# Merge data frames for final results
list2 <- fin4 %>% 
  ungroup() %>% 
  distinct(id, text)

final2 <- fin4 %>% 
  left_join(list2 %>% 
              rename(id1 = id)) %>% 
  left_join(list2 %>% 
              rename(id2.1 = id, text2 = text)) %>% 
  ungroup() %>% 
  group_by(Questionnaire.ITEM) %>% 
  mutate(k = ifelse(all(is.na(fldSkipTo)), 0, 1)) %>% 
  mutate(fil = as.numeric(sub(".*=(\\d+)", "\\1", text2))) %>% 
  mutate(text2 = ifelse(k == 1 & fil == 1 & fldResponseID == 2, NA, text2)) %>% 
  mutate(patternskip = case_when(is.na(text) & is.na(text2) ~ NA,
                                 !is.na(text) & is.na(text2) ~ text,
                                 !is.na(text) & !is.na(text2) ~ paste(text, text2, sep = ",")))

# Filter skip data
skipdata <- fin4 %>% 
  filter(!is.na(fldSkipTo)) %>%
  distinct(Questionnaire.ITEM, id, fldResponseID, fldSkipTo, Variable.name) %>% 
  mutate(item1 = paste(Questionnaire.ITEM, fldResponseID, sep = "=")) %>%
  distinct(id, item1, Variable.name)

# Merge data frames for final results
final <- fin4 %>% 
  left_join(list2 %>% 
              rename(id1 = id)) %>%
  group_by(id2) %>% 
  mutate(id3 = seq(n())) %>% 
  mutate(merge = ifelse(row_number() == which.max(id3) & id3 > 1, 1, 0)) %>% 
  mutate(iditem1 = ifelse(merge == 1, id2, id1)) %>% 
  left_join(skipdata %>% 
              rename(iditem1 = id,
                     name1 = Variable.name)) %>% 
  ungroup() %>% 
  mutate(iditem2 = lag(id2.1)) %>% 
  left_join(skipdata %>% 
              rename(iditem2 = id,
                     item2 = item1,
                     name2 = Variable.name)) %>% 
  select(-c(crossed_column, id1, id2, id2.1, text, id3, merge)) %>% 
  mutate(pattern = ifelse(!is.na(item2), paste(item1, item2, sep = ","), item1)) %>% 
  left_join(fin4 %>% 
              distinct(Questionnaire.ITEM, Variable.name) %>% 
              rename(nameSkip = Variable.name,
                     fldSkipTo = Questionnaire.ITEM)) %>% 
  select("Questionnaire.ITEM", "Variable.name", "id", "fldResponseID", "fldSkipTo",
         "nameSkip", "countpat", "item1", "name1", "item2", "name2", "pattern")

# Create the 'FinalPresent' data frame
FinalPresent <- final %>% 
  left_join(items %>% 
              rename(Questionnaire.ITEM = fldItemID))

# Comment: 
# The code seems to process and clean the data, extracting relevant information from different tables and creating a final dataset. 
# It involves data manipulation, merging, and filtering operations to achieve the desired output.
# It might be helpful to add comments explaining the purpose and logic behind specific steps for better understanding and future maintenance.


# Read the CSV file
dataHC <- read.csv("C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/workNhats/FinalPresentHCStudy.csv") %>% 
  mutate(indicatorResid = ifelse(is.na(indicatorResid),0,indicatorResid)   ) %>% 
  mutate(  Indicator2 =ifelse(is.na(Indicator2),0,Indicator2)   ) %>%
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
                                 ))

write.csv(dataHC, file = "dataHCExampleV02.csv")

dataHC <- read.csv("C:/Users/ccris/Dropbox (University of Michigan)/carlos/Academy/university/Semester IV/745 Practical tools/dataHCExampleV02.csv") %>% 
  mutate(ResID = ifelse(is.na(ResID) | ResID == 0, 0, 1),
         indicatorResid = ifelse(is.na(ResID) | ResID == 0, 0, 1),
         finalJusttexts2 = ifelse(!is.na(pattern) & !is.na(Resid1Inaplicable), str_replace(pattern, "=\\d+", ""), NA),
         Crosswalk = ifelse(!is.na(Crosswalk), sub(",\\s*\\d+$", "", Crosswalk), NA),
         ResIDText = ifelse(Resid1Inaplicable=="1, 2",paste0("resid = ",Resid1Inaplicable),NA ) 
) 

  table(dataHC$Resid1Inaplicable)

write.csv(dataHC, file = "dataHCExampleV06.csv")

dataHC$boxIndicator <- ifelse(dataHC$boxValminus %in% c(-8, -7) | tail(strsplit(as.character(dataHC$case), ",")[[1]], 1) == dataHC$boxValminus, 0, 1)

# Display the structure of the loaded data
print(dataHC)

