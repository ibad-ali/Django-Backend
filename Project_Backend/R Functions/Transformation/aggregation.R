# Required Libraries
library(dplyr)
library(jsonlite)

# Description:
#   Performs grouped aggregation on a data frame.
#
# Parameters:
#   data     - A data frame
#   group_var - Column name(s) to group by (character vector)
#   agg_var  - Column to apply aggregation on (character, required except for "count")
#   agg_func - Aggregation function: "mean", "sum", "min", "max", "count", "median", "sd"
# Returns:
#   A data frame with grouped aggregation applied.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
  stop("Usage: Rscript aggregate.R <input_csv> <group_vars> <agg_var> <agg_func> <output_json>")
}

input_csv <- args[1]
group_vars <- unlist(strsplit(args[2], ","))
agg_var <- args[3]
agg_func <- args[4]
output_json <- args[5]

# Read the input CSV
df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)  # Keeps original column names

# Function to perform grouped aggregation
grouped_aggregation <- function(data, group_var, agg_var = NULL, agg_func = "mean") {
  tryCatch({
    agg_func <- tolower(agg_func)
    
    if (agg_func == "count") {
      aggregated_data <- data %>%
        group_by(across(all_of(group_var))) %>%
        summarise(count = n(), .groups = "drop")
    } else {
      if (is.null(agg_var)) {
        stop("agg_var must be provided and valid for functions other than 'count'.")
      }
      func <- match.fun(agg_func)
      aggregated_data <- data %>%
        group_by(across(all_of(group_var))) %>%
        summarise(agg_value = func(.data[[agg_var]], na.rm = TRUE), .groups = "drop")
    }
    
    return(aggregated_data)
  }, error = function(e) {
    message("Error in grouped_aggregation: ", e$message)
    return(NULL)
  })
}

aggregated_df <- grouped_aggregation(df, group_vars, agg_var, agg_func)

if (is.null(aggregated_df)) {
  stop("Aggregation R script failed.")
}

# Output the processed data as JSON
write(toJSON(aggregated_df, dataframe = "columns", pretty = FALSE, na = "null"), output_json)