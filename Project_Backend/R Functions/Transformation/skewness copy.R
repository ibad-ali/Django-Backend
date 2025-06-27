# Description:
#   Reduces dimensionality using PCA or greedy variable selection (forward or backward).
# Parameters:
#   data        - A data frame
#   method      - "pca" or "greedy"
#   threshold   - For PCA: proportion of variance to retain (0-1)
#                 For greedy: number of features to select or retain
#   target      - Target column name (only for greedy)
#   greedy_mode - "forward" or "backward" (for greedy only)
# Returns:
#   A reduced data.frame or NULL if an error occurs

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript data reduction <input_csv> <column_name> <method> <output_json>")
}

input_csv <- args[1]
method <- args[2]
threshold <- args[3]
target <- args[4]
greedy_mode <- args[5]
output_json <- args[6]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)  # Keeps original column names

reduce_variables <- function(data, method = "pca", threshold = 0.9, target = NULL, greedy_mode = "forward") {
  tryCatch({
    if (!(method %in% c("pca", "greedy"))) stop("Unsupported method: ", method)
    
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    if (ncol(numeric_data) == 0) stop("No numeric columns found for dimensionality reduction.")
    
    if (method == "pca") {
      return(pca_reduction(data, numeric_data, threshold))
    }
    
    if (method == "greedy") {
      return(greedy_reduction(data, numeric_data, threshold, target, greedy_mode))
    }
    
  }, error = function(e) {
    message("Error in reduce_variables: ", e$message)
    return(NULL)
  })
}

# PCA Reduction
pca_reduction <- function(data, numeric_data, threshold) {
  if (any(is.na(numeric_data))) {
    message("Cannot perform PCA: missing values detected in numeric features.")
    return(NULL)
  }
  
  pca_model <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
  var_explained <- cumsum(pca_model$sdev^2 / sum(pca_model$sdev^2))
  num_components <- which(var_explained >= threshold)[1]
  
  pca_data <- as.data.frame(pca_model$x[, 1:num_components])
  colnames(pca_data) <- paste0("PC", 1:num_components)
  
  message(sprintf("PCA retained %d components explaining %.2f%% variance.",
                  num_components, var_explained[num_components] * 100))
  
  non_numeric_data <- data[, setdiff(names(data), names(numeric_data)), drop = FALSE]
  return(cbind(pca_data, non_numeric_data))
}

# Greedy Variable Reduction (Forward/Backward)
# the method ranks features based on how well they relate to the target
greedy_reduction <- function(data, numeric_data, threshold, target, greedy_mode) {
  if (is.null(target) || !(target %in% names(data)))
    stop("Target variable must be specified and present in data for greedy selection.")
  if (!is.numeric(data[[target]]))
    stop("Greedy selection currently supports only numeric target variables.")
  
  features <- setdiff(names(numeric_data), target)
  if (length(features) == 0) stop("No candidate features to select from.")
  
  scores <- sapply(numeric_data[features], function(x) abs(cor(x, data[[target]], use = "complete.obs")))
  
  if (greedy_mode == "forward") {
    threshold <- min(threshold, length(features))
    selected_vars <- names(sort(scores, decreasing = TRUE))[1:threshold]
    message("Greedy forward selection retained variables: ", paste(selected_vars, collapse = ", "))
    return(data[, selected_vars, drop = FALSE])
    
  } else if (greedy_mode == "backward") {
    threshold <- min(threshold, length(features) - 1)
    num_to_keep <- length(features) - threshold
    selected_vars <- names(sort(scores, decreasing = TRUE))[1:num_to_keep]
    message("Greedy backward elimination retained variables: ", paste(selected_vars, collapse = ", "))
    return(data[, selected_vars, drop = FALSE])
    
  } else {
    stop("Invalid greedy_mode. Use 'forward' or 'backward'.")
  }
}

reduced_df <- reduce_variables(df, method, threshold, target, greedy_mode)

if (is.null(reduced_df)) {
  stop("Skewness R script failed.")
}

# Output the processed data as JSON
write(toJSON(reduced_df, dataframe = "columns", pretty = F, na = "null"), output_json)