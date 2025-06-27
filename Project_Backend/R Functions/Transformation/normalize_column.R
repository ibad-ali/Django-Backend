library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript normalize_column.R <input_csv> <column_name> <method> <output_json>")
}

input_csv <- args[1]
column_name <- args[2]
method <- args[3]
output_json <- args[4]

source("R Functions/Transformation/normalization.R")  # where normalize_column is defined

df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)  # Keeps original column names

normalized_df <- normalize_column(df, column_name, method)

if (is.null(normalized_df)) {
  stop("Normalization failed.")
}

write(toJSON(normalized_df, dataframe = "columns", pretty = FALSE, na = "null"), output_json)
