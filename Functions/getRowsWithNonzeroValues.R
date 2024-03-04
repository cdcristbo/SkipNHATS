# Function to create a dataframe with the variable and values
# where '-1' has values different from zero in a contingency table
# targetColumn = "hc1kneesurg"

getRowsWithNonzeroValues <- function(data, targetColumn) {
  # Convert the target column to class "double" if it's not already
  if (!is.numeric(data[[targetColumn]])) {
    data[[targetColumn]] <- as.numeric(data[[targetColumn]])
  }
  
  # Identify the column name ending with "dresid"
  dresid_column <- grep("dresid$", names(data), value = TRUE)
  
  # Create a new dataframe with renamed column
  data2 = data %>% 
    rename(r = dresid_column[1])
  
  # Create the contingency table
  contingency_table <- table(data2$r, data2[[targetColumn]])
  
  # Convert the contingency table into a matrix
  contingency_matrix <- as.matrix(contingency_table)
  
  # Get the row indices where '-1' has values different from zero
  rows_with_nonzero <- which(contingency_matrix[, "-1"] != 0)
  rows_string <- paste(rows_with_nonzero, collapse = ", ")
  
  column_name <- colnames(contingency_matrix)[which(contingency_matrix[4,] != 0)]
  indicatorByResIDstring<- paste(column_name, collapse = ", ")
  
  # Subset the matrix to rows with non-zero sums
  subset_matrix <- contingency_matrix[rows_with_nonzero, -which(colnames(contingency_matrix) == "-1")]
  
  # Check if subset_matrix is a valid matrix with at least two dimensions
  if (is.matrix(subset_matrix) && all(dim(subset_matrix) >= 2)) {
    row_sums <- rowSums(subset_matrix)
    # Identify rows with non-zero sums
    non_zero_rows <- names(which(row_sums != 0))
  } else {
    non_zero_rows <- " "
  }
  
  # Create a dataframe with the variable and the values
  result_df <- data.frame(
    variable = targetColumn,
    round1Inaplicable = rows_string,
    indicatorByResIDValue = indicatorByResIDstring
  )
  
  # Add a column indicating if there are other non-zero rows
  result_df$OtherSkip <- as.integer(any(subset_matrix != 0, na.rm = TRUE))  
  result_df$OtherSkip2 <- paste(non_zero_rows, collapse = ", ")  
  
  # Add the original column name as an additional column
  result_df = result_df %>% 
    mutate(re = dresid_column[1])
  
  return(result_df)
}
