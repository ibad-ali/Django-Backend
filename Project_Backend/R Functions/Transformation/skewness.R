# Description:
#   Applies a transformation to a numeric column to reduce skewness and improve
#   normality. Handles both positive and negative skew.
# Parameters:
#   data   - A data frame
#   column - Name of the column to transform (as string)
#   method - Transformation method to use.
# Supported methods:
#     "auto"       Automatically selects Box-Cox (if x > 0) or Yeo-Johnson
#     "boxcox"     Box-Cox transformation (requires x > 0)
#     "yeojohnson" Yeo-Johnson transformation (handles all real values)
#
#     "log"        log10(x): for strongly right-skewed (x > 0)
#     "neglog"     log10(max(x+1) - x): for strongly left-skewed
#     "log1p"      log1p(x): safer than log, for right skew (x >= 0)
#
#     "sqrt"       sqrt(x): for moderately right-skewed (x >= 0)
#     "negsqrt"    sqrt(max(x+1) - x): for moderately left-skewed
#
#     "inv"        1/x: for severely right-skewed (x ≠ 0)
#     "neginv"     1 / (max(x+1) - x): for severely left-skewed
#
#     "square"     x^2: for nonlinear decreasing response curves
# Returns:
#   Modified data.frame with transformed column, or NULL if error occurs.

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript skewness.R <input_csv> <column_name> <method> <output_json>")
}

input_csv <- args[1]
column_name <- args[2]
method <- args[3]
output_json <- args[4]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)  # Keeps original column names

remove_skewness_column <- function(data, column, method = "auto") {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found in data.")
    if (!is.numeric(data[[column]])) stop("Selected column is not numeric.")
    
    x <- data[[column]]
    
    if (method == "boxcox") {
      if (any(x <= 0)) stop("Box-Cox requires all values to be > 0.")
      trans <- caret::BoxCoxTrans(x)
      data[[column]] <- predict(trans, x)
      
    } else if (method == "yeojohnson") {
      trans <- bestNormalize::yeojohnson(x)
      data[[column]] <- predict(trans)
      
    } else if (method == "log") {
      if (any(x <= 0)) stop("log10 requires x > 0.")
      data[[column]] <- log10(x)
      
    } else if (method == "neglog") {
      data[[column]] <- log10(max(x + 1) - x)
      
    } else if (method == "log1p") {
      if (any(x < 0)) stop("log1p requires x >= 0.")
      data[[column]] <- log1p(x)
      
    } else if (method == "sqrt") {
      if (any(x < 0)) stop("sqrt requires x >= 0.")
      data[[column]] <- sqrt(x)
      
    } else if (method == "negsqrt") {
      data[[column]] <- sqrt(max(x + 1) - x)
      
    } else if (method == "inv") {
      if (any(x == 0)) stop("1/x requires no zero values.")
      data[[column]] <- 1 / x
      
    } else if (method == "neginv") {
      data[[column]] <- 1 / (max(x + 1) - x)
      
    } else if (method == "square") {
      data[[column]] <- x^2
      
    } else if (method == "auto") {
      if (all(x > 0)) {
        trans <- caret::BoxCoxTrans(x)
        data[[column]] <- predict(trans, x)
      } else {
        trans <- bestNormalize::yeojohnson(x)
        data[[column]] <- predict(trans)
      }
      
    } else {
      stop("Unknown transformation method: ", method)
    }
    
    return(data)
  }, error = function(e) {
    message("Error in remove_skewness_column: ", e$message)
    return(NULL)
  })
}

skewed_df <- remove_skewness_column(df, column_name, method)

if (is.null(skewed_df)) {
  stop("Skewness R script failed.")
}

# Output the processed data as JSON
write(toJSON(skewed_df, dataframe = "columns", pretty = F, na = "null"), output_json)

# References:
# https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/
# https://anshikaaxena.medium.com/how-skewed-data-can-skrew-your-linear-regression-model-accuracy-and-transfromation-can-help-62c6d3fe4c53