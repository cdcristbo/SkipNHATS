library(dplyr)
library(tidyr)
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/workNhats/"
load(paste0(base_path, "outcomes/FinalPresent.RData"))

patternDataB <- patternData %>%
    filter(grepl("BOX", Questionnaire.ITEM)) #%>%
    #filter(Questionnaire.ITEM == "BOX HC4")

patternDataB$if_col <- str_extract(patternDataB$`tblQuestionText-QuestionText - EN`, "(?<=If ).*?(?= =)")
patternDataB$go_col <- str_extract(patternDataB$`tblQuestionText-QuestionText - EN`, "(?<=go to ).*?(?=\\.)")
patternDataB$final_col <- str_extract(patternDataB$`tblQuestionText-QuestionText - EN`, "(?<=Otherwise, go to ).*?(?=\\.)")

patternDataBox = patternDataB %>% 
  select(c("Questionnaire.ITEM","id","fldSectionID","tblItem-Numb","tblQuestionText-QuestionText - EN",
           "if_col","go_col","final_col" )) %>%
  left_join(fullList %>% 
              select(`tblItem-ItemTag`,`tblItem-Numb`) %>% 
              rename(id_go = `tblItem-Numb`,
                     go_col =`tblItem-ItemTag` ) %>% 
              mutate(base_go_col = str_extract(go_col, "^[^_]*")) , 
            by = c("go_col" = "base_go_col")) %>% 
  left_join(fullList %>% 
              select(`tblItem-ItemTag`,`tblItem-Numb`) %>% 
              rename(id_final = `tblItem-Numb`,
                     final_col =`tblItem-ItemTag` ) %>% 
              mutate(base_final_col = str_extract(final_col, "^[^_]*")) , 
            by = c("final_col" = "base_final_col")) %>% 
  select(-c(go_col,final_col)) %>% 
  rename(go_col = go_col.y,
         final_col = final_col.y) %>% 
  left_join(
    trueNames %>% 
      select("Questionnaire ITEM","Variable name" ) %>% 
      rename(go_col = "Questionnaire ITEM",
             name_go = "Variable name")) %>% 
  mutate(if_col = toupper(if_col)) %>% 
  left_join(
    trueNames %>% 
      select("Questionnaire ITEM","Variable name" ) %>% 
      rename(final_col = "Questionnaire ITEM",
             name_final = "Variable name")) %>% 
  left_join(
    trueNames %>% 
      select("Questionnaire ITEM","Variable name" ) %>% 
      rename(if_col = "Questionnaire ITEM",
             name_if = "Variable name")) 
  
# if patternData$id_go is greater than patternData$id_final, then the pattern is a skip pattern is the following:
# 1. create a dataframe like box = data.frame(table(data$hc1fllsinmth,data$hc1faleninyr))
# 2. save the values of the row  box$var1 == data$if_col2 into a vector called stringbox = paste(box$var1, collapse = ",")
# 3. return a dataframe valled returnBox with the variable and the values variable = targetColumn and boxVal = stringbox

# Function to create a dataframe with variables and values based on skip patterns
patternData = patternDataBox[10,]
targetColumn="BOX HC14"
results_box = NULL

for (i in 1:nrow(patternDataBox)) {
  targetColumn = patternDataBox$Questionnaire.ITEM[i]  
  patternData2 = patternDataBox[i,]
  resultsbox <- getBox(data, patternData2, targetColumn)
  results_box <- rbind(results_box, resultsbox )
  #print(resultsbox)
}
#write.csv(results_box,file = paste0(base_path, "outcomes/Boxes.csv")) 

getBox(data, patternData, targetColumn = "BOX HC14")

getBox <- function(data, patternData, targetColumn) {

    # Identify the column name ending with "dresid"
    dresid_column <- grep("dresid$", names(data), value = TRUE)
    
    # Extract integer value from the dresid_column
    cond <- sub("dresid", "", dresid_column[1])
    integer_value <- as.integer(sub("r", "", cond))
    
    # Rename the column to "r" for consistency
    data2 <- data %>% 
      rename(r = dresid_column[1]) %>%
      filter(r!=4)

    # Extract relevant information from the patternData
    if (patternData$id_go > patternData$id_final) {
        # Create a dataframe like box = data.frame(table(data$hc1fllsinmth, data$hc1faleninyr))
        box <- data.frame(table(data2[[patternData$name_if]], data2[[patternData$name_final]]))
        
        boxTable = box %>% 
            filter(Var1 == 1 & Freq > 0)

       boxTable2 = box %>%       
            filter(Var2 == -1 & Freq > 0)

       stringbox <- paste(boxTable$Var2, collapse = ",")
       stringbox2 <- paste(boxTable2$Var1, collapse = ",")     
            returnBox <- data.frame(
            variable = targetColumn,
            boxVal = stringbox,
            boxValminus = stringbox2,
            name1 = patternData$if_col,
            name2 = patternData$go_col,
            name3 = patternData$final_col,
            case =  1
        )
        
        return(returnBox)
    } else {
       box <- data.frame(table(data2[[patternData$name_if]], data2[[patternData$name_go]]))
       
       boxTable = box %>%       
            filter(Var1 == 2 & Freq > 0)
       boxTable2 = box %>%       
            filter(Var2 == -1 & Freq > 0)

       stringbox <- paste(boxTable$Var2, collapse = ",")
       stringbox2 <- paste(boxTable2$Var1, collapse = ",")     
            returnBox <- data.frame(
            variable = targetColumn,
            boxVal = stringbox,
            boxValminus = stringbox2,
            name1 = patternData$if_col,
            name2 = patternData$go_col,
            name3 = patternData$final_col,
            case = 2
        )
        
        return(returnBox)
    }
}

# Assuming you have a dataset called 'data' and 'patternData' as mentioned in the code
#View(patternData2)


#result <- getBox(data, patternData, targetColumn = "BOX HC4")





# pastego = patternDataB %>%  
#   #filter(grepl("HC3", Questionnaire.ITEM)) %>% 
#   select(c(Questionnaire.ITEM, Variable.name,id)) %>% 
#   distinct() %>% 
#   mutate(go_col = str_extract(Questionnaire.ITEM, "^[^_]*"))
# 
# pastefinal = patternDataB %>%  
#   #filter(grepl("HC3", Questionnaire.ITEM)) %>% 
#   select(c(Questionnaire.ITEM, Variable.name,id)) %>% 
#   distinct() %>% 
#   mutate(final_col = str_extract(Questionnaire.ITEM, "^[^_]*"))

# patternDataBox = patternDataB %>% 
#   select(c("Questionnaire.ITEM","id","fldSectionID","tblItem-Numb","if_col",
#          "if_col","go_col","final_col" )) %>% 
#   select(-id) %>% 
#   left_join(pastego,by = "go_col" ) %>% 
#   left_join(pastefinal,by = "final_col" ) 
# 
# for (i in 1:11) {
#    targetColumn = patternDataBox$Questionnaire.ITEM[i]  
#    patternData2 = patternDataBox %>% 
#       filter(Questionnaire.ITEM == targetColumn)
#     resultsbox <- getBox(data, patternData2, targetColumn)
#     results_box <- rbind(results_box, resultsbox )
#     #print(resultsbox)
# }
# 
# data.frame(results_box)  
#  
#  