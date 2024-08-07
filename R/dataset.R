#' Read a dataset from a CSV file.
#'
#' This function reads a CSV file and returns the dataset as a data frame.
#'
#' @param dataset_path The path to the CSV file.
#' @return A data frame representing the dataset.
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
#' for numerical features or categories for categorical features.
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


#' Calculate the dimension of the problem based on feature information.
#'
#' This function takes a list of feature information and calculates the dimension
#' based on the type of each feature. Method is inspired by referenced paper.
#'
#' @param feature_info A list containing information about each feature.
#'
#' @return The calculated dimension based on the feature types.
#'
#' @references
#' Fister, I., Iglesias, A., Galvez, A., Del Ser, J., Osaba, E., & Fister, I. (2018).
#' Differential evolution for association rule mining using categorical and numerical attributes.
#' In \emph{Intelligent Data Engineering and Automated Learning--IDEAL 2018: 19th International Conference, Madrid, Spain, November 21--23, 2018, Proceedings, Part I} (pp. 79--88). Springer.
#'
#' @export
problem_dimension <- function(feature_info) {
  # permutation + 1 for cut point
  dimension <- length(feature_info) + 1
  for (col in names(feature_info)) {
    if (feature_info[[col]]$type == "numerical") {
      dimension <- dimension + 3
    } else {
      dimension <- dimension + 2
    }
  }

  return(dimension)
}
