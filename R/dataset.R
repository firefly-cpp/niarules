#' Read a dataset from a CSV file.
#'
#' This function reads a CSV file and returns the dataset as a data frame.
#'
#' @param dataset_path The path to the CSV file.
#' @return A data frame representing the dataset.
#' @examples
#' \dontrun{
#' read_dataset("path/to/your/dataset.csv")
#' }
#'
#' @export
read_dataset <- function(dataset_path) {
  dataset <- read.csv(dataset_path)
  return(dataset)
}

# Function to extract feature information
#' Extract feature information from a dataset.
#'
#' This function analyzes the given dataset and extracts information about each feature.
#'
#' @param data The dataset to analyze.
#' @return A list containing information about each feature.
#' @export
extract_feature_info <- function(data) {
  columns <- colnames(data)
  feature_info <- list()

  for (col in columns) {
    if (is.numeric(data[[col]])) {
      lower_bound <- min(data[[col]], na.rm = TRUE)
      upper_bound <- max(data[[col]], na.rm = TRUE)

      feature_info[[col]] <- list(type = "numerical", lower_bound = lower_bound, upper_bound = upper_bound)
    } else {
      categories <- unique(data[[col]])
      feature_info[[col]] <- list(type = "categorical", categories = categories)
    }
  }

  return(feature_info)
}

# Function to print feature information
#' Print feature information extracted from a dataset.
#'
#' This function prints the information extracted about each feature.
#'
#' @param feature_info The list containing information about each feature.
#' @export
print_feature_info <- function(feature_info) {
  columns <- names(feature_info)

  for (col in columns) {
    cat("Feature:", col, "\n")
    cat("  Type:", feature_info[[col]]$type, "\n")

    if (feature_info[[col]]$type == "numerical") {
      cat("  Lower Bound:", feature_info[[col]]$lower_bound, "\n")
      cat("  Upper Bound:", feature_info[[col]]$upper_bound, "\n")
    } else {
      cat("  Categories:", toString(feature_info[[col]]$categories), "\n")
    }

    cat("\n")
  }
}
