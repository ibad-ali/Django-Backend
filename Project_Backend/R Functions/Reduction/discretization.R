# Description:
#   Discretizes a continuous numeric column in a data frame using the specified method.
#   Supports binning, k-means clustering, and entropy-based (supervised) discretization.
# Parameters:
#   data       - A data frame
#   column     - Name of the numeric column to discretize (as string)
#   method     - Discretization method: "equalwidth", "equalfreq", "kmeans", or "entropy"
#   bins       - Number of bins/clusters (if applicable)
#   target     - Optional target variable name (required for "entropy" method)
# Returns:
#   Modified data.frame with a new factor column "<column>_discretized", or NULL if error occurs

#install.packages("jsonlite")

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
df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)


discretize_column <- function(data, column, method = "equalwidth", bins = 4, target = NULL) {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found in data.")
    if (!is.numeric(data[[column]])) stop("Selected column is not numeric.")
    if (!(method %in% c("equalwidth", "equalfreq", "kmeans", "entropy"))) {
      stop("Unsupported discretization method: ", method)
    }
    
    x <- data[[column]]
    new_col <- paste0(column, "_discretized")
    
    if (method == "equalwidth") {
      data[[new_col]] <- cut(x, breaks = bins, labels = FALSE, include.lowest = TRUE)
      
    } else if (method == "equalfreq") {
      quantiles <- quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
      data[[new_col]] <- cut(x, breaks = unique(quantiles), labels = FALSE, include.lowest = TRUE)
      
    } else if (method == "kmeans") {
      clusters <- stats::kmeans(x, centers = bins, iter.max = 50)$cluster
      data[[new_col]] <- as.factor(clusters)
      
    } else if (method == "entropy") {
      if (is.null(target)) stop("Target variable is required for entropy-based discretization.")
      if (!target %in% names(data)) stop("Target column not found in data.")
      
      if (!requireNamespace("FSelectorRcpp", quietly = TRUE)) {
        stop("Please install 'FSelectorRcpp' package.")
      }
      
      if (!is.factor(data[[target]])) {
        data[[target]] <- as.factor(data[[target]])
      }
      
      # Use formula interface: target ~ column
      formula <- as.formula(paste(target, "~", column))
      disc_df <- FSelectorRcpp::discretize(formula, data)
      
      # Add new column
      data[[new_col]] <- disc_df[[column]]
    }
    
    data[[new_col]] <- as.factor(data[[new_col]])
    return(data)
    
  }, error = function(e) {
    message("Error in discretize_column: ", e$message)
    return(NULL)
  })
}

discretized_df <- discretize_column(df, column_name, method)

if (is.null(discretized_df)) {
  stop("Discretization failed.")
}

# Output the processed data as JSON
write(toJSON(discretized_df, dataframe = "columns", pretty = F, na = "null"),output_json)

# Reference:
# https://www.rdocumentation.org/packages/FSelectorRcpp/versions/0.3.13/topics/discretize