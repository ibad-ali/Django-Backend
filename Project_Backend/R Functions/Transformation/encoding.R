# Description:
#   Encodes a categorical column in a data frame using the specified encoding method.
#   Supports one-hot encoding and label encoding.
# Parameters:
#   data   - A data frame
#   column - Name of the column to encode (as string)
#   method - Encoding method to use.
# Supported methods:
#     "onehot"     One-Hot Encoding (creates dummy variables for each category)
#     "label"      Label Encoding (converts categories to integer codes)
# Returns:
#   Modified data frame with encoded column(s), or NULL if error occurs.

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript encoding.R <input_csv> <column_name> <method> <output_json>")
}

input_csv <- args[1]
column_name <- args[2]
method <- args[3]
output_json <- args[4]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE)

# Function to encode the categorical column
encode_categorical_column <- function(data, column, method = "onehot") {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found in data.")
    if (!is.factor(data[[column]]) && !is.character(data[[column]])) {
      stop("Selected column must be a factor or character.")
    }

    x <- as.factor(data[[column]])

    if (method == "onehot") {
      # One-hot encoding using model.matrix
      dummies <- model.matrix(~ x - 1)  # Remove intercept
      colnames(dummies) <- gsub("^x", paste0(column, "_"), colnames(dummies))
      data <- cbind(data, dummies)
      data[[column]] <- NULL  # Remove original column

    } else if (method == "label") {
      # Label encoding: convert factors to integers
      data[[column]] <- as.integer(x)

    } else {
      stop("Unknown encoding method: ", method)
    }

    return(data)
  }, error = function(e) {
    message("Error in encode_categorical_column: ", e$message)
    return(NULL)
  })
}

encoded_df <- encode_categorical_column(df, column_name, method)

if (is.null(encoded_df)) {
  stop("Encoding R script failed.")
}

# Output the processed data as JSON
write(toJSON(encoded_df, dataframe = "columns", pretty = F, na = "null"), output_json)