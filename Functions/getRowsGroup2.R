# result <- getRowsGroup2(data, patternData[i, ], patternData[i, ]$Variable.name, patternData[i, ]$name2)
# data 
# patternData = patternData[i, ] 
# targetColumn = patternData[i, ]$Variable.name
# skipVariable = patternData[i, ]$name2

# Function to create a dataframe with variables and values based on skip patterns
getRowsGroup2 <- function(data, patternData, targetColumn, skipVariable) {
  
  # Extract relevant information from the patternData
  info <- patternData %>% 
    #filter(Variable.name == targetColumn & name2 == skipVariable) %>% 
    mutate(numberskip1 = as.numeric(str_extract(item1, "(?<=\\=)\\d+"))) %>%
    mutate(numberskip2 = as.numeric(str_extract(item2, "(?<=\\=)\\d+"))) %>%
    distinct(Variable.name, Questionnaire.ITEM, item1, name1, item2, name2, pattern, numberskip1, numberskip2) %>% 
    mutate(
      name1 = str_extract(pattern, "^[^=]*"),
      numberskip1 = as.numeric(str_extract(pattern, "(?<=\\=)\\d+(?=,)")),
      name2 = str_extract(pattern, "(?<=,)\\w+(?=\\=)"),
      numberskip2 = as.numeric(str_extract(pattern, "\\d+$"))
    )
  
  # Check if targetColumn and skipVariable are in the column names of the data
  if (targetColumn %in% names(data) & skipVariable %in% names(data)) {
    
    # Convert targetColumn and skipVariable to class "double" if not already
    if (!is.numeric(data[[targetColumn]])) {
      data[[targetColumn]] <- as.numeric(data[[targetColumn]])
    }
    if (!is.numeric(data[[skipVariable]])) {
      data[[skipVariable]] <- as.numeric(data[[skipVariable]])
    }
    
    # Identify the column name ending with "dresid"
    dresid_column <- grep("dresid$", names(data), value = TRUE)
    
    # Extract integer value from the dresid_column
    cond <- sub("dresid", "", dresid_column[1])
    integer_value <- as.integer(sub("r", "", cond))
    
    # Rename the column to "r" for consistency
    data2 <- data %>% 
      rename(r = dresid_column[1])
    
    # Filter data based on skip patterns
    if (integer_value == 1) {
      filter_column <- as.character(tail(info$name1, 1))
      
      # Filter data2 based on the last value in info$name2
      contingency_table3 <- data2 %>%
        filter(r != 4) %>% 
        filter(if_all(all_of(filter_column), ~. != info$numberskip1) ) %>% 
        filter(if_all(all_of(filter_column), ~. != -8) )
    
      } else if (integer_value > 1) {
      filter_column <- as.character(tail(info$name2, 1))
      
      # Filter data2 based on the last value in info$name2
      contingency_table3 <- data2 %>%
        filter(!(r %in% c(6, 8))) %>% 
        filter(if_all(all_of(filter_column), ~. != info$numberskip2)) %>% 
        filter(if_all(all_of(filter_column), ~. != -8) )
    }
    
    
    # Create a contingency table for skipVariable and targetColumn
    tableSkipValues <- table(contingency_table3[[info$Variable.name]], contingency_table3[[info$name2]])
    
    # Check if "-1" is in the row names of tableSkipValues
    if ("-1" %in% rownames(tableSkipValues)) {
      outcome <- paste(names(which(tableSkipValues["-1",] != 0)), collapse = ",")
    } else {
      outcome <- NA
    }
    
    # Filter the tableSkipValues dataframe based on numberskip2 and non-zero frequency
    tableFreq <- data.frame(tableSkipValues) %>% 
      filter(Var2 == info$numberskip2 & Freq != 0)
    
    # Create a dataframe with the variable and the values
    result_df <- data.frame(
      variable = info$Variable.name,
      priorvariable = info$name2,
      skipPrior = outcome,
      minus1Target = paste(tableFreq$Var1, collapse = ",")
    )
  }
  
  return(result_df)
}
