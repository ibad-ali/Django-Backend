library(readxl)
library(jsonlite)

classify_columns_advanced <- function(file_path, sheet = 1, max_unique_cat = 10, has_headers = TRUE) {
  # Read file based on extension
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    if (has_headers) {
      data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE, header = FALSE)
      # Generate column names as numbers
      names(data) <- as.character(1:ncol(data))
    }
  } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE) || grepl("\\.xls$", file_path, ignore.case = TRUE)) {
    if (has_headers) {
      data <- readxl::read_excel(file_path, sheet = sheet, .name_repair = "none")
    } else {
      data <- readxl::read_excel(file_path, sheet = sheet, col_names = FALSE)
      # Generate column names as numbers
      names(data) <- as.character(1:ncol(data))
    }
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
  file_path <- args[1]
  has_headers <- if (length(args) > 1) as.logical(args[2]) else TRUE
  result <- classify_columns_advanced(file_path, has_headers = has_headers)
  # Convert to JSON and print (this will be captured by Python)
  cat(jsonlite::toJSON(result, auto_unbox = TRUE))
}