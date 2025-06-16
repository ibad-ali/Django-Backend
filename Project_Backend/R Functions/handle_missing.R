handle_missing <- function(df, column, method) {
  if (!column %in% colnames(df)) {
    stop("Selected column not found in dataframe")
  }

  if (method == "mean") {
    df[[column]][is.na(df[[column]])] <- mean(df[[column]], na.rm = TRUE)
  } else if (method == "median") {
    df[[column]][is.na(df[[column]])] <- median(df[[column]], na.rm = TRUE)
  } else if (method == "mode") {
    mode_val <- as.numeric(names(sort(table(df[[column]]), decreasing = TRUE))[1])
    df[[column]][is.na(df[[column]])] <- mode_val
  } else if (method == "remove_col") {
    df[[column]] <- NULL
  } else if (method == "remove_row") {
    df <- df[!is.na(df[[column]]), ]
  } else {
    stop("Invalid method")
  }

  return(df)
}
