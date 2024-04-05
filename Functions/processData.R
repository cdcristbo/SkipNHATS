
#patternData5 <- processData(Part2, fullList, trueNames,section="HT")

# Extract relevant columns from 'fullList'
processData <- function(Part2, fullList, trueNames,section) {
  # Extract relevant columns from 'fullList'
  complete <- fullList %>%
    select("tblItem-Category_Id", "tblItem-ItemTag", "tblItem-Numb") %>%
    rename(fldSectionID = `tblItem-Category_Id`,
           fldItemID = `tblItem-ItemTag`)
  
  # Filter and join relevant data from 'complete', 'Part2', and 'trueNames'
items <- complete %>%
    #filter(fldSectionID != "IS") %>%
    left_join(Part2 %>% 
                select(-fldResponseSchemeName)) %>%
    filter(fldSectionID == section) %>%
    left_join(trueNames %>%
                select(`Variable name`,`Questionnaire ITEM`) %>% 
                rename(fldItemID = `Questionnaire ITEM`) %>%
                distinct()) %>%
    mutate(textSkip = ifelse(!is.na(fldSkipTo) , paste0( `Variable name` ,"=",fldResponseID), NA)) %>%
    group_by(fldSkipTo) %>% 
    #mutate(text3 = paste(textSkip,collapse = ",")) %>% 
    mutate(text2 = ifelse(all(is.na(textSkip)), NA, paste(textSkip, collapse = ','))) %>% 
    ungroup() %>% 
    select(-fldResponseID) %>% 
    distinct()  %>% 
  arrange(fldSkipTo) %>% 
  group_by(fldItemID) %>%
  
    # summarise(length(unique(`Variable name`))) #verify that the values are unique IMPORTANT!!!!!!!!!!!!!!!!!!!!
    #mutate(text2 = paste(textSkip,sep = " or ")) %>% 
    select(-c(textSkip)) %>% 
    mutate(index = seq(n())) %>%
  
    filter(index==1) %>% 
    ungroup() %>% 
  arrange(`tblItem-Numb`) %>% 
    mutate(postionInitial= seq(n())) %>% 
    
  select(-c(`tblItem-Numb`,index)) %>% 
  mutate(fldSkipTo = toupper(fldSkipTo),
         fldItemID =toupper(fldItemID)  )
    
ItemSKips = items %>% 
  filter(text2!="NA") %>% 
  left_join(items %>% 
              distinct(fldItemID,postionInitial) %>% 
              rename(postionFinal = postionInitial,
                    fldSkipTo =  fldItemID))

  
FinalItems = items %>% 
  select(-postionInitial) %>% 
  left_join(ItemSKips) %>% 
  mutate(postionInitial = postionInitial+1,
         postionFinal = postionFinal-1) %>% 
  mutate(text2 =ifelse(text2=="NA",NA,text2))
  

data <- FinalItems %>%
  mutate(
    postionInitial = replace_na(postionInitial, -1),
    postionFinal = replace_na(postionFinal, -1),
    pattern = NA_character_
  )
# %>% 
  # select(-`Variable name`) %>% 
  # left_join(trueNames %>% 
  #             distinct(`Questionnaire ITEM`,`Variable name`) %>% 
  #             rename(fldItemID = `Questionnaire ITEM`))


for (i in seq_len(nrow(data))) {
  if (data$postionInitial[i] > 0 & data$postionFinal[i] > 0) {
    data$pattern[data$postionInitial[i]:data$postionFinal[i]] <- data$text2[i]
  }
}

FinalPresent = data %>% 
  rename(Questionnaire.ITEM=fldItemID) %>% 
mutate(Variable.name = `Variable name`) %>% 
  mutate(item1 = pattern,
         id = seq(n()),
         "tblItem-Numb" = id,
         "Variable label" = Variable.name) %>% 
  left_join(data %>% 
              distinct(fldItemID,`Variable name`) %>% 
              rename(fldSkipTo =fldItemID,
                     nameSkip = `Variable name`)) %>% 
  mutate(  name1Del = str_extract(pattern, "^[^=]*")  ) %>% 
  left_join(data %>% 
              distinct(fldItemID,`Variable name`) %>% 
              rename(name1Del =fldItemID,
                     name1 = `Variable name`)) %>% 
  select(-c("text2","postionInitial","postionFinal","name1Del")) %>% 
  mutate(
    fldResponseID = NA,
    countpat = NA,
    item2 = NA,
    name2 = NA,
    `tblQuestionText-QuestionText - EN` = NA,
    `tblQuestionText-QuestionText - ES` = NA,
    fldResponseSchemeName = NA,
    `SP Public File` = NA,
    `OP Public File` = NA,
    `Tracker File` = NA,
    `Restricted (R) or Sensitive (S) Variable` = NA,
    `Not on File` = NA
  ) %>% 
  mutate(
    pattern = str_replace(pattern, `Variable name`, ""),
    pattern = str_replace(pattern, ",=.*", ""),
    pattern = case_when(
      str_count(item1, ",") > 1 ~ item1,
      TRUE ~ pattern
  )
)

  return(FinalPresent)
}
