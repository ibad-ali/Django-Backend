# Description:
#   Functions to detect and remove outliers from both numeric and categorical data.
#   Supports multiple strategies: Z-score, IQR, DBSCAN clustering (multivariate),
#   and rare category detection (for categorical variables).
#
# Parameters:
#   data     - A data frame
#   column   - Name of the column to process (as string, if applicable)
#   action   - "remove" (default) or "replace"
#   method-specific options (see below)
#
# Supported methods:
#   remove_outliers_zscore:
#     - column (numeric), threshold (default: 2.5)
#     - Good for symmetric distributions, large datasets; sensitive to skew
#
#   remove_outliers_iqr:
#     - column (numeric), factor (default: 1.5)
#     - Robust for skewed distributions
#
#   remove_outliers_dbscan:
#     - eps (default: 0.5), minPts (default: 5)
#     - Multivariate numeric clustering-based method; cannot handle missing values
#
#   remove_rare_categories:
#     - column (categorical), threshold, type ("proportion" or "count")
#     - Detects and handles rare labels in factor/character columns
#
# TODO:
#   - Decide: If we remove outliers from one column, should we remove the entire row?
#
# Returns:
#   A modified data.frame with outliers removed or replaced, or NULL if an error occurs.

# Load required libraries
if (!requireNamespace("dbscan", quietly = TRUE)) install.packages("dbscan")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(dbscan)
library(ggplot2)

# Removes numeric outliers based on Z-score threshold.
# Parameters: column (numeric), threshold (default: 2.5)
# Not good for small datasets. Z-score values would also be less than default threashold for outliers
# Need to adjust threshold accordingly for dataset size
# Good for Symmetric distributions and larger datasets
# Skew-sensitive
remove_outliers_zscore <- function(data, column, threshold = 2.5, action = c("remove", "replace")) {
  tryCatch({
    action <- match.arg(action)
    if (!column %in% names(data)) stop("Column '", column, "' not found.")
    if (!is.numeric(data[[column]])) stop("Column '", column, "' must be numeric for Z-score method.")
    
    z_scores <- scale(data[[column]])
    outliers <- abs(z_scores) > threshold
    removed_count <- sum(outliers, na.rm = TRUE)
    
    if (removed_count == 0) {
      cat("No Z-score outliers found in", column, "\n")
      return(data)
    }
    
    if (action == "remove") {
      cat("Removing", removed_count, "outlier(s) from", column, "using Z-score.\n")
      return(data[!outliers, ])
    } else {
      non_na_vals <- data[[column]][!is.na(data[[column]]) & !outliers]
      mean_val <- mean(non_na_vals)
      if (all(non_na_vals == floor(non_na_vals))) mean_val <- round(mean_val)
      data[[column]][outliers] <- mean_val
      cat("Replacing", removed_count, "outlier(s) in", column, "with mean:", mean_val, "\n")
      return(data)
    }
  }, error = function(e) {
    cat("Error in remove_outliers_zscore:", e$message, "\n")
    return(NULL)
  })
}

# Remove outliers using the IQR method
# Good when data is skewed
# IQR Method Finds outliers based on the range between the 1st and 3rd quartile.
remove_outliers_iqr <- function(data, column, factor = 1.5, action = c("remove", "replace")) {
  tryCatch({
    action <- match.arg(action)
    if (!column %in% names(data)) stop("Column '", column, "' not found.")
    if (!is.numeric(data[[column]])) stop("Column '", column, "' must be numeric for IQR method.")
    
    col_data <- data[[column]]
    Q1 <- quantile(col_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower <- Q1 - factor * IQR_value
    upper <- Q3 + factor * IQR_value
    outliers <- which(col_data < lower | col_data > upper)
    
    if (length(outliers) == 0) {
      cat("No IQR outliers found in", column, "\n")
      return(data)
    }
    
    if (action == "remove") {
      cat("Removing", length(outliers), "outlier(s) from", column, "using IQR method.\n")
      return(data[-outliers, ])
    } else {
      non_outlier_vals <- col_data[!is.na(col_data) & !(col_data < lower | col_data > upper)]
      mean_val <- mean(non_outlier_vals)
      if (all(non_outlier_vals == floor(non_outlier_vals))) mean_val <- round(mean_val)
      data[[column]][outliers] <- mean_val
      cat("Replacing", length(outliers), "outlier(s) in", column, "with mean:", mean_val, "\n")
      return(data)
    }
  }, error = function(e) {
    cat("Error in remove_outliers_iqr:", e$message, "\n")
    return(NULL)
  })
}

# Removes multi-variate numeric outliers using DBSCAN clustering.
# DBSCAN (Density-Based Spatial Clustering)
# Identifies dense clusters and treats points in low-density regions as outliers.
# Parameters: numeric_columns (vector), eps (default: 0.5), minPts (default: 5)
remove_outliers_dbscan <- function(data, eps = 0.5, minPts = 5, action = c("remove", "replace")) {
  tryCatch({
    action <- match.arg(action)
    numeric_columns <- names(data)[sapply(data, is.numeric)]
    if (length(numeric_columns) < 1) stop("No numeric columns available for DBSCAN.")
    
    subset_data <- data[, numeric_columns, drop = FALSE]
    if (anyNA(subset_data)) stop("DBSCAN cannot run with NA values. Please impute missing values first.")
    
    scaled_data <- scale(subset_data)
    clustering <- dbscan::dbscan(scaled_data, eps = eps, minPts = minPts)
    outlier_rows <- which(clustering$cluster == 0)
    
    if (length(outlier_rows) == 0) {
      cat("No DBSCAN outliers found.\n")
      return(data)
    }
    
    cat("Found", length(outlier_rows), "DBSCAN outlier(s) across:", paste(numeric_columns, collapse = ", "), "\n")
    
    if (action == "remove") {
      return(data[-outlier_rows, ])
    } else {
      for (col in numeric_columns) {
        outlier_vals <- data[outlier_rows, col]
        non_outlier_vals <- data[-outlier_rows, col]
        mean_val <- round(mean(non_outlier_vals, na.rm = TRUE))
        to_replace <- which(outlier_vals > mean_val)
        if (length(to_replace) > 0) {
          cat("Replacing outliers in column '", col, "' (", 
              paste(outlier_vals[to_replace], collapse = ", "), 
              ") with mean:", mean_val, "\n")
          data[outlier_rows[to_replace], col] <- mean_val
        }
      }
      return(data)
    }
  }, error = function(e) {
    cat("Error in remove_outliers_dbscan:", e$message, "\n")
    return(NULL)
  })
}

# Removes rows with rare categories in a categorical column.
# Parameters: column (character/factor), threshold = 0.1 means <10% frequency
remove_rare_categories <- function(data, column, threshold = 0.1, type = c("proportion", "count"), action = c("remove", "replace")) {
  tryCatch({
    if (!column %in% names(data)) stop("Column '", column, "' not found.")
    if (!is.character(data[[column]]) && !is.factor(data[[column]])) stop("Column '", column, "' must be categorical.")
    
    type <- match.arg(type)
    action <- match.arg(action)
    
    freq_table <- table(data[[column]])
    if (type == "proportion") {
      freq_prop <- prop.table(freq_table)
      rare_cats <- names(freq_prop[freq_prop < threshold])
    } else {
      rare_cats <- names(freq_table[freq_table < threshold])
    }
    
    if (length(rare_cats) == 0) {
      cat("No rare categories found in column '", column, "'.\n")
      return(data)
    }
    
    if (action == "remove") {
      removed_rows <- sum(data[[column]] %in% rare_cats)
      cat("Removing", removed_rows, "row(s) with rare categories in '", column, "':", paste(rare_cats, collapse = ", "), "\n")
      return(data[!data[[column]] %in% rare_cats, , drop = FALSE])
    } else {
      common_cat <- names(which.max(freq_table[!(names(freq_table) %in% rare_cats)]))
      cat("Replacing rare category/ies in '", column, "':", paste(rare_cats, collapse = ", "), "with '", common_cat, "'\n")
      data[[column]][data[[column]] %in% rare_cats] <- common_cat
      return(data)
    }
  }, error = function(e) {
    cat("Error in remove_rare_categories:", e$message, "\n")
    return(NULL)
  })
}

# References:
# # https://medium.com/@amit25173/dbscan-in-r-3c93c97b674b