# Description:
#   Performs numeric operations between two specified columns in a data frame.
# Parameters:
#   data_file    - Path to input CSV file
#   column1      - First column name
#   column2      - Second column name
#   operation    - Operation to perform (i.e., "add", "subtract", "multiply", "divide", "mod")
#   new_column   - Name of the new column to create in the data frame
#   output_file  - Path to output JSON file

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6) {
  stop("Usage: Rscript variable_construction.R <input_csv> <column1> <column2> <operation> <new_column> <output_json>")
}

input_csv <- args[1]
column1 <- args[2]
column2 <- args[3]
operation <- args[4]
new_column <- args[5]
output_json <- args[6]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE)

# Function to perform operations
perform_operation <- function(data, col1, col2, op, new_col) {
  tryCatch({
    v1 <- data[[col1]]
    v2 <- data[[col2]]
    
    if (!is.numeric(v1) || !is.numeric(v2)) {
      stop("Selected columns must be numeric.")
    }

    result <- switch(op,
                     add = v1 + v2,
                     subtract = v1 - v2,
                     multiply = v1 * v2,
                     divide = ifelse(v2 == 0, NA, v1 / v2),
                     mod = ifelse(v2 == 0, NA, v1 %% v2),
                     stop("Unknown operation: ", op))
    
    data[[new_col]] <- result
    
    return(data)
  }, error = function(e) {
    message("Error in perform_operation: ", e$message)
    return(NULL)
  })
}

constructed_df <- perform_operation(df, column1, column2, operation, new_column)

if (is.null(constructed_df)) {
  stop("Variable construction R script failed.")
}

# Output the processed data as JSON
write(toJSON(constructed_df, dataframe = "columns", pretty = F, na = "null"), output_json)