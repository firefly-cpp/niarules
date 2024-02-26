#' Build rules based on a candidate solution.
#'
#' This function takes a candidate solution vector and a features list and builds rule.
#'
#' @param solution The solution vector.
#' @param features The features list.
#' @return A rule.
#' @export
#'
#' @examples
#' solution <- c(0.2, 0.8, 0.5)
#' features <- list(
#'   feature1 = list(type = "numerical", lower_bound = 0, upper_bound = 1),
#'   feature2 = list(type = "categorical", categories = c("A", "B", "C")),
#'   feature3 = list(type = "numerical", lower_bound = 0, upper_bound = 1)
#' )
#' rules <- build_rule(solution, features)
#'
#'
build_rule <- function(solution, features) {
  is_first_rule <- TRUE
  rules <- list()

  len <- length(solution)
  permutation <- solution[(len - length(features) + 1):len]
  solution <- solution[-((len - length(features) + 1):len)]
  permutation <- order(permutation, decreasing = TRUE)

  for (i in permutation) {
    feat_names <- names(features)
    feature <- feat_names[i]
    vector_position <- feature_position(features, feature)
    threshold_position <- vector_position + 1

    if (solution[vector_position] > solution[threshold_position]) {
      feat_type <- features[[feature]]$type

      if (feat_type != "categorical") {
        border1 <- calculate_border(features[[feature]], solution[vector_position])
        border2 <- calculate_border(features[[feature]], solution[threshold_position])

        if (border1 > border2) {
          temp <- border1
          border1 <- border2
          border2 <- temp
        }

        if (is_first_rule) {
          rules <- add_attribute(list(), feature, feat_type, border1, border2, "EMPTY")
          is_first_rule <- FALSE
        } else {
          rules <- add_attribute(rules, feature, feat_type, border1, border2, "EMPTY")
        }
      } else {
        categories <- features[[feature]]$categories
        selected <- calculate_selected_category(solution[vector_position], length(categories))

        if (is_first_rule) {
          rules <- add_attribute(list(), feature, feat_type, 1.00, 1.00, categories[selected])
          is_first_rule <- FALSE
        } else {
          rules <- add_attribute(rules, feature, feat_type, 1.00, 1.00, categories[selected])
        }
      }
    } else {
      # Attributes under the threshold - continue
      next
    }
  }

  return(rules)
}

#' Get the position of a feature.
#'
#' This function returns the position of a feature in the vector,
#' considering the type of the feature.
#'
#' @param features The features list.
#' @param feature The name of the feature to find.
#' @return The position of the feature.
#' @export
#'
#' @examples
#' features <- list(
#'   feature1 = list(type = "numerical"),
#'   feature2 = list(type = "categorical"),
#'   feature3 = list(type = "numerical")
#' )
#' position <- feature_position(features, "feature2")
#'
feature_position <- function(features, feature) {
  position <- 0
  feat_names <- names(features)

  for (feat_name in feat_names) {
    dtype <- features[[feat_name]]$type
    position <- ifelse(dtype == "categorical", position + 1, position + 2)

    if (feat_name == feature) {
      break
    }
  }

  return(position)
}

#' Add an attribute to the "rule" list.
#'
#' This function adds an attribute to the existing list.
#'
#' @param rules The current rules list.
#' @param name The name of the feature in the rule.
#' @param type The type of the feature in the rule.
#' @param border1 The first border value in the rule.
#' @param border2 The second border value in the rule.
#' @param value The value associated with the rule.
#' @return The updated rules list.
#' @export
#'
#' @examples
#' rules <- list()
#' new_rules <- add_attribute(rules, "feature1", "numerical", 0.2, 0.8, "EMPTY")
#'
add_attribute <- function(rules, name, type, border1, border2, value) {
  new_rule <- list(
    name = name,
    type = type,
    border1 = border1,
    border2 = border2,
    value = value
  )
  return(c(rules, list(new_rule)))
}

#' Calculate the border value based on feature information and a given value.
#'
#' This function calculates the border value for a feature based on the
#' feature information and a given value.
#'
#' @param feature_info Information about the feature.
#' @param value The value to calculate the border for.
#' @return The calculated border value.
#' @export
#'
#' @examples
#' feature_info <- list(type = "numerical", lower_bound = 0, upper_bound = 1)
#' border_value <- calculate_border(feature_info, 0.5)
#'
calculate_border <- function(feature_info, value) {
  lower_bound <- feature_info$lower_bound
  upper_bound <- feature_info$upper_bound
  result <- value * (upper_bound - lower_bound) + lower_bound
   #TODO check
  return(result)
}

#' Calculate the selected category based on a value and the number of categories.
#'
#' This function calculates the selected category based on a given value and
#' the total number of categories.
#'
#' @param value The value to calculate the category for.
#' @param num_categories The total number of categories.
#' @return The calculated selected category.
#' @export
#'
#' @examples
#' selected_category <- calculate_selected_category(0.3, 5)
#'
calculate_selected_category <- function(value, num_categories) {
  # TODO (fix borders in EVOLUTIONARY PROCESS)
  selected <- trunc(round(value * num_categories))
  return(ifelse(selected == 0, 1, selected))
}
