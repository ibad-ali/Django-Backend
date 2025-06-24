# Description:
#   Handles missing values in a specified column using various imputation methods.
#   Supports both simple statistical imputations and predictive modeling approaches.
#   Automatically handles numeric and categorical columns where applicable.
#
# Parameters:
#   data           - A data frame
#   column         - Name of the column to impute (as string)
#   method         - Imputation method to apply (default is "mean")
#   constant_value - Required if using method = "constant"; fixed value for imputation
#
# Supported methods:
#   "mean"       - Numeric only             → Replace with column mean
#   "median"     - Numeric only             → Replace with column median
#   "constant"   - Numeric / Categorical    → Replace with user-defined value
#   "mode"       - Numeric / Categorical    → Replace with most frequent value
#   "regression" - Numeric only             → Predict using linear regression
#   "rpart"      - Numeric / Categorical    → Predict using decision tree (rpart)
#
# Notes:
#   - Predictive methods ("regression", "rpart") require complete predictor variables.
#   - If predictors for regression have missing values, prediction will return NA.
#     These predictors must be imputed first, and the function will print diagnostics.
#
# Returns:
#   A modified data.frame with missing values imputed, or NULL if an error occurs.

handle_missing_values <- function(data, column, method = "mean", constant_value = NA) {
  tryCatch({
    # Input validation
    if (!column %in% names(data)) {
      stop(paste("Column", column, "not found in dataset"))
    }
    
    col_data <- data[[column]]
    is_num <- is.numeric(col_data)
    
    if (method %in% c("mean", "median", "regression") && !is_num) {
      stop(paste(method, "imputation requires numeric column"))
    }
    
    # Predictive Imputation Methods
    if (method %in% c("regression", "rpart")) {
      complete_rows <- data[!is.na(col_data), ]
      missing_rows <- data[is.na(col_data), ]
      
      if (nrow(complete_rows) == 0 || nrow(missing_rows) == 0) {
        cat("No imputable missing values for method:", method, "\n")
        return(data)
      }
      
      predictors <- setdiff(names(data), column)
      formula <- as.formula(paste(column, "~", paste(predictors, collapse = " + ")))
      
      if (method == "regression") {
        model <- lm(formula, data = complete_rows)
        preds <- predict(model, newdata = missing_rows)
        
        cat("Imputing", column, "using LINEAR REGRESSION. Predicted values:\n")
        print(preds)
        
        # Handle failed predictions due to NA in predictors
        na_pred_mask <- is.na(preds)
        successful_rows <- which(!na_pred_mask)
        failed_rows <- which(na_pred_mask)
        
        if (length(successful_rows) > 0) {
          data[[column]][as.numeric(rownames(missing_rows)[successful_rows])] <- preds[successful_rows]
        }
        
        if (length(failed_rows) > 0) {
          cat("The following rows could not be imputed due to missing predictor values:\n")
          for (i in failed_rows) {
            row_id <- as.numeric(rownames(missing_rows)[i])
            missing_row <- missing_rows[i, , drop = FALSE]
            na_predictors <- predictors[which(is.na(missing_row[, predictors]))]
            cat(sprintf(" - Row %d: Missing predictor(s): %s\n",
                        row_id, paste(na_predictors, collapse = ", ")))
          }
          cat("Recommendation: Impute these predictor(s) before retrying regression imputation.\n\n")
        }
        
      } else {
        model <- rpart::rpart(formula, data = complete_rows,
                              method = if (is_num) "anova" else "class")
        preds <- predict(model, newdata = missing_rows, type = if (is_num) "vector" else "class")
        
        cat("Imputing", column, "using DECISION TREE (rpart). Predicted values:\n")
        print(preds)
      }
      
      # Replace missing with predicted values
      data[[column]][is.na(col_data)] <- preds
      return(data)
    }
    
    # Simple Imputation Methods
    impute_value <- switch(
      method,
      mean = {
        val <- mean(col_data, na.rm = TRUE)
        cat("Imputing", column, "with MEAN:", val, "\n")
        val
      },
      median = {
        val <- median(col_data, na.rm = TRUE)
        cat("Imputing", column, "with MEDIAN:", val, "\n")
        val
      },
      constant = {
        if (is.na(constant_value)) {
          stop("Must provide a constant_value for 'constant' method.")
        }
        if (is_num && !is.numeric(constant_value)) {
          stop("Constant value must be numeric for numeric column. Please provide a numeric value.")
        }
        if (!is_num && !is.character(constant_value)) {
          stop("Constant value must be a string for categorical column. Please provide a string value.")
        }
        cat("Imputing", column, "with CONSTANT:", constant_value, "\n")
        constant_value
      },
      mode = {
        tbl <- table(col_data)
        val <- names(which.max(tbl))  # Pick most frequent
        cat("Imputing", column, "with MODE:", val, "\n")
        val
      },
      stop("Unsupported imputation method:", method)
    )
    
    # Apply simple imputation
    data[[column]][is.na(col_data)] <- impute_value
    return(data)
    
  }, error = function(e) {
    cat("Error in handle_missing_values:", e$message, "\n")
    return(NULL)
  })
}