#' Read a Dataset from a CSV File, Including Time Series Data
#'
#' This function reads a CSV file and returns the dataset as a data frame.
#' If a timestamp column is detected, it ensures the column is correctly parsed
#' as a date-time object in POSIXct format.
#'
#' @param dataset_path A string specifying the path to the CSV file.
#' @param timestamp_col Optional. A string specifying the name of the column containing timestamps.
#'   Default is `"timestamp"`.
#' @param timestamp_formats Optional. A vector of strings specifying the formats of the timestamp in the dataset.
#'   Default is `c("%d/%m/%Y %H:%M:%S", "%H:%M:%S %d/%m/%Y")`.
#'
#' @return A data frame representing the dataset. If a timestamp column is provided and exists, it will be parsed
#'   as a POSIXct date-time object.
#'
#' @details
#' If the `timestamp_col` is provided and exists in the dataset, this function attempts to parse
#' it into a date-time format using the specified `timestamp_formats`. If parsing fails, an error
#' will be thrown.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Read a dataset without timestamps
#' data <- read_dataset("path/to/dataset.csv")
#'
#' # Read a dataset with timestamps
#' data_with_timestamps <- read_dataset(
#'   "path/to/timeseries.csv",
#'   timestamp_col = "timestamp",
#'   timestamp_formats = c("%d/%m/%Y %H:%M:%S", "%H:%M:%S %d/%m/%Y")
#' )
#' }
#'
#' @export
read_dataset <- function(dataset_path,
                         timestamp_col = "timestamp",
                         timestamp_formats = c("%d/%m/%Y %H:%M:%S", "%H:%M:%S %d/%m/%Y")) {
  # Read the dataset
  dataset <- read.csv(dataset_path, stringsAsFactors = FALSE)

  # Check if the timestamp column is specified and exists
  if (!is.null(timestamp_col) && timestamp_col %in% colnames(dataset)) {
    parsed_timestamp <- NULL

    # Try parsing the timestamp with each format in order
    for (fmt in timestamp_formats) {
      parsed_timestamp <- tryCatch({
        as.POSIXct(dataset[[timestamp_col]], format = fmt, tz = "UTC")
      }, error = function(e) NULL)

      if (!is.null(parsed_timestamp) && !any(is.na(parsed_timestamp))) {
        # Successfully parsed
        dataset[[timestamp_col]] <- parsed_timestamp
        break
      }
    }

    # If all formats failed
    if (is.null(parsed_timestamp) || any(is.na(dataset[[timestamp_col]]))) {
      stop("Error: Unable to parse timestamps in column '", timestamp_col,
           "'. None of the provided formats matched. Check the timestamp formats.")
    }
  }

  return(dataset)
}


#' Extract feature information from a dataset, excluding timestamps.
#'
#' This function analyzes the given dataset and extracts information about each feature.
#'
#' @param data The dataset to analyze.
#' @param timestamp_col Optional. The name of the timestamp column to exclude from features.
#' @return A list containing information about each feature, including type and bounds/categories.
#' @export
extract_feature_info <- function(data, timestamp_col = "timestamp") {
  columns <- setdiff(colnames(data), timestamp_col)  # Exclude timestamp column
  feature_info <- list()

  for (col in columns) {
    if (is.numeric(data[[col]])) {
      lower_bound <- min(data[[col]], na.rm = TRUE)
      upper_bound <- max(data[[col]], na.rm = TRUE)

      feature_info[[col]] <- list(
        type = "numerical",
        lower_bound = lower_bound,
        upper_bound = upper_bound
      )
    } else {
      categories <- unique(data[[col]])
      feature_info[[col]] <- list(type = "categorical", categories = categories)
    }
  }

  return(feature_info)
}

#' Print feature information extracted from a dataset.
#'
#' This function prints the information extracted about each feature.
#'
#' @param feature_info The list containing information about each feature.
#' @export
#'
#' @return
#' A message is printed to the console for each feature, providing information
#' about the feature's type, and additional details such as lower and upper bounds
#' for numerical features, or categories for categorical features.
#' No explicit return value is generated.
#'
print_feature_info <- function(feature_info) {
  columns <- names(feature_info)

  for (col in columns) {
    message("Feature:", col)
    message("  Type:", feature_info[[col]]$type)

    if (feature_info[[col]]$type == "numerical") {
      message("  Lower Bound:", feature_info[[col]]$lower_bound)
      message("  Upper Bound:", feature_info[[col]]$upper_bound)
    } else {
      message("  Categories:", toString(feature_info[[col]]$categories))
    }

    message("")
  }
}

#' Calculate the dimension of the problem, excluding timestamps.
#'
#' @param feature_info A list containing information about each feature.
#' @param is_time_series Boolean indicating if time series data is present.
#'
#' @return The calculated dimension based on the feature types.
#' @export
problem_dimension <- function(feature_info, is_time_series = FALSE) {
  dimension <- length(feature_info) + 1  # +1 for the cut point

  for (col in names(feature_info)) {
    if (feature_info[[col]]$type == "numerical") {
      dimension <- dimension + 3  # Numerical features add 3 dimensions
    } else {
      dimension <- dimension + 2  # Categorical features add 2 dimensions
    }
  }

  # Add two extra dimensions for time series (start and end times)
  if (is_time_series) {
    dimension <- dimension + 2
  }

  return(dimension)
}
