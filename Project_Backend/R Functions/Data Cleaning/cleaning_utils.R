# Description:
#   Standardizes date formats in a specified column of a data frame.
#   Attempts to parse multiple common date formats and converts them into
#   a consistent Date or formatted character representation.
# Parameters:
#   data          - A data frame
#   column        - Name of the column to transform (as string)
#   output_format - Optional output format (e.g., "%Y-%m-%d"). If NULL, returns Date.
# Returns:
#   Modified data.frame with standardized date column, or NULL if error occurs.
standardize_date_column <- function(data, column, output_format = NULL) {
  tryCatch({
    if (!column %in% names(data)) {
      stop(paste("Column", column, "not found in data"))
    }
    original_values <- data[[column]]
    parsed_dates <- parse_date_time(
      original_values,
      orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY"),
      quiet = TRUE
    )
    
    # Warn if some could not be parsed
    if (any(is.na(parsed_dates) & !is.na(original_values))) {
      warning(sprintf(
        "Some dates in column '%s' could not be parsed. They remain NA.",
        column
      ))
    }
    
    # Convert based on output format
    if (!is.null(output_format)) {
      data[[column]] <- ifelse(!is.na(parsed_dates),
                               format(parsed_dates, output_format),
                               NA_character_)
    } else {
      data[[column]] <- as.Date(parsed_dates)
    }
    return(data)
  }, error = function(e) {
    message("Error in standardize_date_column: ", e$message)
    return(NULL)
  })
}

# Description:
#   Standardizes text case in a character column of a data frame.
#   Useful for making values consistently lower, upper, or title case.
# Parameters:
#   data   - A data frame
#   column - Name of the column to transform (as string)
#   case   - Desired case: "lower", "upper", or "title"
# Returns:
#   Modified data.frame with standardized case, or NULL if error occurs.
standardize_case <- function(data, column, case = "lower") {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found.")
    if (!is.character(data[[column]])) stop("Column must be character.")
    
    data[[column]] <- switch(
      case,
      lower = tolower(data[[column]]),
      upper = toupper(data[[column]]),
      title = tools::toTitleCase(data[[column]]),
      stop("Unsupported case option. Choose: lower, upper, or title.")
    )
    return(data)
  }, error = function(e) {
    message("Error in standardize_case: ", e$message)
    return(NULL)
  })
}

# Description:
#   Removes leading and trailing whitespace from entries in a character column.
#   Helpful for cleaning inconsistent categorical data.
# Parameters:
#   data   - A data frame
#   column - Name of the column to trim (as string)
# Returns:
#   Modified data.frame with trimmed text values, or NULL if error occurs.
trim_whitespace <- function(data, column) {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found.")
    if (!is.character(data[[column]])) stop("Column must be character.")
    
    data[[column]] <- trimws(data[[column]])
    return(data)
  }, error = function(e) {
    message("Error in trim_whitespace: ", e$message)
    return(NULL)
  })
}