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
if (length(args) != 6) {
  stop("Usage: Rscript data reduction <input_csv> <column_name> <method> <output_json>")
}

input_csv <- args[1]
method <- args[2]
threshold <- args[3]
target <- args[4]
greedy_mode <- args[5]
output_json <- args[6]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

reduce_variables <- function(data, method = "pca", threshold = 0.9, target = NULL, greedy_mode = "forward") {
  tryCatch({
    if (!(method %in% c("pca", "greedy_forward", "greedy_backward", "stepwise"))) stop("Unsupported method: ", method)
    
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    if (ncol(numeric_data) == 0) stop("No numeric columns found for dimensionality reduction.")
    
    if (method == "pca") {
      return(pca_reduction(data, numeric_data, threshold))
    }
    
    if (method %in% c("pca", "greedy_forward", "greedy_backward", "stepwise")) {
      
      return(greedy_reduction(data, target, greedy_mode))
    }
    
  }, error = function(e) {
    message("Error: ", e$message)
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

greedy_reduction <- function(data, target, mode = c("forward", "backward", "both"), trace = FALSE) {
  # Load MASS
  if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
  library(MASS)
  
  mode <- match.arg(mode)
  
  # Escape column names with backticks for formula safety
  safe_target <- paste0("`", target, "`")
  predictors <- setdiff(names(data), target)
  safe_predictors <- paste0("`", predictors, "`", collapse = " + ")
  
  # Create full formula
  full_formula <- as.formula(paste(safe_target, "~", safe_predictors))
  
  # Fit base models
  full_model <- lm(full_formula, data = data)
  intercept_model <- lm(as.formula(paste(safe_target, "~ 1")), data = data)
  
  # Check AIC of full model for backward/both mode
  if (mode %in% c("backward") && !is.finite(AIC(full_model))) {
    stop("Full model has -Inf AIC (likely a perfect fit). Backward selection cannot proceed.")
  }
  
  # Run stepwise selection
  result_model <- switch(mode,
                         forward = stepAIC(intercept_model,
                                           scope = list(lower = intercept_model, upper = full_model),
                                           direction = "forward", trace = trace),
                         backward = stepAIC(full_model,
                                            scope = list(lower = intercept_model, upper = full_model),
                                            direction = "backward", trace = trace),
                         both = stepAIC(full_model,
                                        scope = list(lower = intercept_model, upper = full_model),
                                        direction = "both", trace = trace)
  )
  
  # Extract selected variables (without backticks)
  selected_vars <- all.vars(formula(result_model))[-1]  # Drop target
  
  # Return subset of data with original column names
  return(data[, c(target, selected_vars), drop = FALSE])
}

# Greedy Variable Reduction (Forward/Backward)
greedy_reduction_old <- function(data, target, greedy_mode = c("forward", "backward", "stepwise")) {
  
  # Check inputs
  if (!(target %in% colnames(data))) {
    stop("Target variable not found in the data.")
  }
  
  greedy_mode <- match.arg(greedy_mode)
  
  # Exclude target from explanatory variables
  explanatory_vars <- setdiff(colnames(data), target)
  
  # Create the full and null models
  formula_full <- as.formula(paste(target, "~", paste(explanatory_vars, collapse = "+")))
  null_model <- lm(as.formula(paste(target, "~ 1")), data = data)
  full_model <- lm(formula_full, data = data)
  
  result <- NULL
  
  if (greedy_mode == "backward") {
    result <- step(full_model, direction = "backward", trace = FALSE)
  } else if (greedy_mode == "forward") {
    result <- step(null_model, scope = list(lower = null_model, upper = formula_full), direction = "forward", trace = FALSE)
  } else if (greedy_mode == "stepwise") {
    result <- step(full_model, direction = "both", trace = FALSE)
  }
  
  # Extract selected features from the resulting model
  selected_features <- names(coef(result))[-1] # Exclude intercept
  
  # Return a dataframe containing only the selected features and the target
  return(data[, c(selected_features, target)])
}

reduced_df <- reduce_variables(df, method, threshold, target, greedy_mode)

if (is.null(reduced_df)) {
  stop("Script failed.")
}

# Output the processed data as JSON
write(toJSON(reduced_df, dataframe = "columns", pretty = F, na = "null"), output_json)