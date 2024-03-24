# Function to create a dataframe with variables and values based on skip patterns
# This function takes in three parameters: 'data' (the input data frame), 'patternData' (a data frame containing skip pattern information), and 'targetColumn' (the name of the target column).
# It identifies a column ending with "dresid" in the 'data' data frame, extracts an integer value from it, renames the column to "r" for consistency, and filters the data frame.
# Then, it extracts relevant information from the 'patternData' data frame based on certain conditions.
# Depending on the conditions, it creates a new data frame 'box' using the 'table' function, filters 'box' based on additional conditions, combines values into strings, and creates a final data frame 'returnBox' with the desired variables and values.
# The function returns the 'returnBox' data frame.
getBox <- function(data, patternData, targetColumn) {
    
    # Identify the column name ending with "dresid"
    dresid_column <- grep("dresid$", names(data), value = TRUE)
    
    # Extract integer value from the dresid_column
    cond <- sub("dresid", "", dresid_column[1])
    integer_value <- as.integer(sub("r", "", cond))
    
    # Rename the column to "r" for consistency
    data2 <- data %>% 
        rename(r = dresid_column[1]) %>%
        filter(r != 4)
    
    # Extract relevant information from the patternData
    if (patternData$id_go > patternData$id_final) {
        # Create a dataframe like box = data.frame(table(data$hc1fllsinmth, data$hc1faleninyr))
        box <- data.frame(table(data2[[patternData$name_if]], data2[[patternData$name_final]]))
        
        # Filter box based on conditions
        boxTable <- box %>% 
            filter(Var1 == patternData$if_col2 & Freq > 0)
        
        boxTable2 <- box %>%       
            filter(Var2 == -1 & Freq > 0)
        
        # Combine values into a string
        stringbox <- paste(boxTable$Var2, collapse = ",")
        stringbox2 <- paste(boxTable2$Var1, collapse = ",")     
        
        # Create and return the dataframe
        returnBox <- data.frame(
            variable = targetColumn,
            boxVal = stringbox,
            boxValminus = stringbox2,
            case =  1
        )
        
        return(returnBox)
    } else {
        box <- data.frame(table(data2[[patternData$name_if]], data2[[patternData$name_go]]))
        
        boxTable <- box %>%       
            filter(Var1 == 2 & Freq > 0)
        boxTable2 <- box %>%       
            filter(Var2 == -1 & Freq > 0)
        
        # Combine values into a string
        stringbox <- paste(boxTable$Var2, collapse = ",")
        stringbox2 <- paste(boxTable2$Var1, collapse = ",")     
        
        # Create and return the dataframe
        returnBox <- data.frame(
            variable = targetColumn,
            boxVal = stringbox,
            boxValminus = stringbox2,
            case = 2
        )
        
        return(returnBox)
    }
}
