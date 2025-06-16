library(readxl)
library(jsonlite)

classify_columns_advanced <- function(file_path, sheet = 1, max_unique_cat = 10) {
  # Read file based on extension
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)  # Keeps original column names
  } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE) || grepl("\\.xls$", file_path, ignore.case = TRUE)) {
    data <- readxl::read_excel(file_path, sheet = sheet, .name_repair = "none")  # Prevents name modification
  } else {
    stop("Unsupported file type. Only .csv, .xlsx, and .xls are supported.")
  }

  qualitative_cols <- character()
  quantitative_cols <- character()

  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    non_na_data <- col_data[!is.na(col_data)]

    if (length(non_na_data) == 0) next

    if (is.numeric(col_data)) {
      if (length(unique(non_na_data)) > max_unique_cat) {
        quantitative_cols <- c(quantitative_cols, col_name)
      } else {
        qualitative_cols <- c(qualitative_cols, col_name)
      }
    } else if (is.logical(col_data)) {
      qualitative_cols <- c(qualitative_cols, col_name)
    } else if (is.character(col_data) || is.factor(col_data)) {
      if (length(unique(non_na_data)) <= max_unique_cat) {
        qualitative_cols <- c(qualitative_cols, col_name)
      } else {
        qualitative_cols <- c(qualitative_cols, col_name)
      }
    } else if (inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
      qualitative_cols <- c(qualitative_cols, col_name)
    } else {
      qualitative_cols <- c(qualitative_cols, col_name)
    }
  }

  return(list(
    qualitative_columns = qualitative_cols,
    quantitative_columns = quantitative_cols
  ))
}

# Main execution when called from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  result <- classify_columns_advanced(args[1])
  # Convert to JSON and print (this will be captured by Python)
  cat(jsonlite::toJSON(result, auto_unbox = TRUE))
}