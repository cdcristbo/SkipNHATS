library(dplyr)
library(tidyr)
base_path <- "C:/Users/ccris/Dropbox (University of Michigan)/carlos/Work/Nhats/workNhats/"
load(paste0(base_path, "outcomes/FinalPresent.RData"))

patternData <- FinalPresent %>%
    filter(grepl("BOX", Questionnaire.ITEM)) #%>%
    #filter(Questionnaire.ITEM == "BOX HC4")
# if patternData$id_go is greater than patternData$id_final, then the pattern is a skip pattern is the following:
# 1. create a dataframe like box = data.frame(table(data$hc1fllsinmth,data$hc1faleninyr))
# 2. save the values of the row  box$var1 == data$if_col2 into a vector called stringbox = paste(box$var1, collapse = ",")
# 3. return a dataframe valled returnBox with the variable and the values variable = targetColumn and boxVal = stringbox

# Function to create a dataframe with variables and values based on skip patterns
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
            filter(Var1 == patternData$if_col2 & Freq > 0)

       boxTable2 = box %>%       
            filter(Var2 == -1 & Freq > 0)

       stringbox <- paste(boxTable$Var2, collapse = ",")
       stringbox2 <- paste(boxTable2$Var1, collapse = ",")     
            returnBox <- data.frame(
            variable = targetColumn,
            boxVal = stringbox,
            boxValminus = stringbox2,
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
            case = 2
        )
        
        return(returnBox)
    }
}

# Assuming you have a dataset called 'data' and 'patternData' as mentioned in the code
View(patternData2)
result <- getBox(data, patternData, targetColumn = "BOX HC4")
results_box3 = results_box
results_box = NULL
for (i in 1:3) {
   targetColumn = patternData$Questionnaire.ITEM[i]  
   patternData2 = patternData %>% 
      filter(Questionnaire.ITEM == targetColumn)
    resultsbox <- getBox(data, patternData2, targetColumn)
    results_box <- rbind(results_box, resultsbox )
    #print(resultsbox)
}

data.frame(results_box)  
 
 