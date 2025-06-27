# Description:
#   Normalizes a numeric column in a data frame using one of several methods.
#   Useful for feature scaling prior to modeling or visualization.
#
# Parameters:
#   data        - A data frame
#   column_name - Name of the numeric column to normalize (character)
#   method      - Normalization method: 
#                 "minmax"  = scales to [0, 1] range
#                 "zscore"  = centers to mean 0, sd 1
#                 "decimal" = moves decimal based on max magnitude
#                 "robust"  = centers using median and IQR (robust to outliers)
#
# Returns:
#   Modified data.frame with the specified column normalized.
#   Returns NULL if an error occurs.

normalize_column <- function(data, column_name, method = "minmax") {
  tryCatch({
    method <- tolower(method)
    valid_methods <- c("minmax", "zscore", "decimal", "robust")
    
    # Check for column existence
    if (!(column_name %in% colnames(data))) {
      stop(paste("Column", column_name, "not found in dataset."))
    }
    
    # Check for numeric type
    if (!is.numeric(data[[column_name]])) {
      stop(paste("Column", column_name, "is not numeric. Normalization not applicable."))
    }
    
    # Validate method
    if (!(method %in% valid_methods)) {
      stop(paste("Invalid method. Choose one of:", paste(valid_methods, collapse = ", ")))
    }
    
    # Define normalization method
    normalize_func <- switch(method,
                             "minmax" = function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
                             "zscore" = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
                             "decimal" = function(x) {
                               j <- ceiling(log10(max(abs(x), na.rm = TRUE)))
                               x / (10^j)
                             },
                             "robust" = function(x) (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
    )
    
    message(paste("Normalizing column:", column_name, "using method:", method))
    
    # Apply transformation
    data[[column_name]] <- normalize_func(data[[column_name]])
    
    return(data)
  }, error = function(e) {
    message("Error in normalize_column: ", e$message)
    return(NULL)
  })
}