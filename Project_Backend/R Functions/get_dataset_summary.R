suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(moments)
  library(jsonlite)
})

get_dataset_summary <- function(file_path, sheet = 1, max_unique_cat = 10) {
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
  } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE) || grepl("\\.xls$", file_path, ignore.case = TRUE)) {
    data <- read_excel(file_path, sheet = sheet)
  } else {
    stop("Unsupported file type.")
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)
  n_duplicates <- sum(duplicated(data))

  qualitative_cols <- character()
  quantitative_cols <- character()

  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    non_na_data <- col_data[!is.na(col_data)]

    if (length(non_na_data) == 0) next

    if (is.numeric(col_data)) {
      if (length(unique(non_na_data)) > max_unique_cat) {
        quantitative_cols <- c(quantitative_cols, col_name)
      } else {
        qualitative_cols <- c(qualitative_cols, col_name)
      }
    } else if (is.logical(col_data) || is.character(col_data) || is.factor(col_data) || inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
      qualitative_cols <- c(qualitative_cols, col_name)
    } else {
      qualitative_cols <- c(qualitative_cols, col_name)
    }
  }

  column_stats <- lapply(names(data), function(col_name) {
    col_data <- data[[col_name]]
    non_na_data <- col_data[!is.na(col_data)]
    is_num <- is.numeric(col_data)
    mf <- sort(table(non_na_data), decreasing = TRUE)

    skewness_val <- if (is_num && length(non_na_data) > 0) skewness(non_na_data, na.rm = TRUE) else NA

    outliers <- if (is_num && length(non_na_data) > 0) {
      q1 <- quantile(non_na_data, 0.25, na.rm = TRUE)
      q3 <- quantile(non_na_data, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      sum(col_data < lower_bound | col_data > upper_bound, na.rm = TRUE)
    } else NA

    list(
      column_name = col_name,
      type = class(col_data)[1],
      missing = sum(is.na(col_data)),
      distinct = length(unique(non_na_data)),
      zeros = if (is_num) sum(col_data == 0, na.rm = TRUE) else NA,
      mean = if (is_num) mean(non_na_data, na.rm = TRUE) else NA,
      median = if (is_num) median(non_na_data, na.rm = TRUE) else NA,
      sd = if (is_num) sd(non_na_data, na.rm = TRUE) else NA,
      min = if (is_num) min(non_na_data, na.rm = TRUE) else NA,
      max = if (is_num) max(non_na_data, na.rm = TRUE) else NA,
      iqr = if (is_num) IQR(non_na_data, na.rm = TRUE) else NA,
      skewness = skewness_val,
      outliers = outliers,
      most_frequent = if (length(mf) > 0) names(mf)[1] else NA,
      freq_count = if (length(mf) > 0) as.integer(mf[1]) else NA
    )
  })

  frequency_data <- list()

  for (col_name in qualitative_cols) {
    col_data <- data[[col_name]]
    col_data <- col_data[!is.na(col_data)]
    if (length(col_data) == 0) next

    freq_table <- sort(table(col_data), decreasing = TRUE)
    top_freq <- head(freq_table, 10)
    total <- sum(top_freq)
    percentages <- round((top_freq / total) * 100, 2)

    frequency_data[[col_name]] <- list(
      type = "categorical",
      labels = names(percentages),
      values = as.numeric(percentages)
    )
  }

  for (col_name in quantitative_cols) {
    col_data <- data[[col_name]]
    col_data <- col_data[!is.na(col_data)]
    if (length(col_data) == 0) next

    hist_data <- hist(col_data, plot = FALSE, breaks = 10)
    total <- sum(hist_data$counts)
    percentages <- round((hist_data$counts / total) * 100, 2)

    frequency_data[[col_name]] <- list(
      type = "numerical",
      breaks = hist_data$breaks,
      percentages = percentages
    )
  }

  cor_matrix <- NULL
  if (length(quantitative_cols) > 1) {
    numeric_data <- data[, quantitative_cols, drop = FALSE]
    numeric_data <- numeric_data[, colSums(is.na(numeric_data)) < nrow(numeric_data), drop = FALSE]

    if (ncol(numeric_data) > 1) {
      raw_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      cor_matrix <- list(
        columns = colnames(raw_matrix),
        matrix = unname(as.matrix(raw_matrix))
      )
    }
  }

  return(list(
    summary = list(
      num_rows = n_rows,
      num_columns = n_cols,
      num_qualitative_columns = length(qualitative_cols),
      num_quantitative_columns = length(quantitative_cols),
      num_duplicates = n_duplicates
    ),
    columns = column_stats,
    frequency_data = frequency_data,
    correlation_matrix = cor_matrix
  ))
}

# Main execution
args <- commandArgs(trailingOnly = TRUE)
file_path <- args[1]
result <- get_dataset_summary(file_path)
cat(toJSON(result, auto_unbox = TRUE, na = "null"))
