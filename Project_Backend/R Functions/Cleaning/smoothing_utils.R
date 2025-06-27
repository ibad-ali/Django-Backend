# Description:
#   Smooths a numeric column in a data frame using either binning (mean over fixed intervals)
#   or regression-based methods like LOESS or linear fit.
# Parameters:
#   data            - A data frame
#   column          - Name of the numeric column to smooth (as string)
#   method          - Smoothing method: "binning" or "regression"
#   bin_size        - Number of points in each bin (only used for binning)
#   regression_type - Type of regression: "loess" (default) or "linear"
# Returns:
#   Modified data.frame with smoothed column, or NULL if error occurs.

smooth_data <- function(data, column, method = "binning", bin_size = 5, regression_type = "loess") {
  tryCatch({
    if (!column %in% names(data)) stop("Column not found.")
    if (!is.numeric(data[[column]])) stop("Column must be numeric.")
    
    x <- seq_along(data[[column]])
    y <- data[[column]]
    
    if (method == "binning") {
      # Apply simple mean binning over fixed intervals
      smoothed <- y
      n <- length(y)
      for (i in seq(1, n, by = bin_size)) {
        idx <- i:min(i + bin_size - 1, n)
        bin_mean <- mean(y[idx], na.rm = TRUE)
        smoothed[idx] <- bin_mean
      }
      data[[column]] <- smoothed
      cat("Applied binning (mean within bins of size", bin_size, ") to column:", column, "\n")
      
    } else if (method == "regression") {
      # Apply regression-based smoothing (LOESS or linear)
      if (regression_type == "loess") {
        model <- loess(y ~ x)
        data[[column]] <- predict(model)
        cat("Applied LOESS regression smoothing to column:", column, "\n")
        
      } else if (regression_type == "linear") {
        model <- lm(y ~ x)
        data[[column]] <- predict(model)
        cat("Applied LINEAR regression smoothing to column:", column, "\n")
        
      } else {
        stop("Unsupported regression type. Choose 'loess' or 'linear'.")
      }
      
    } else {
      stop("Unsupported smoothing method. Choose 'binning' or 'regression'.")
    }
    
    return(data)
  }, error = function(e) {
    message("Error in smooth_data: ", e$message)
    return(NULL)
  })
}