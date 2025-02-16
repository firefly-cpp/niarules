#' Evaluate a candidate solution, with optional time series filtering.
#'
#' This function takes a candidate solution (vector), a list of features,
#' and instances. It evaluates the fitness of an association rule by calculating
#' support and confidence. If time series data is used, restrict evaluation
#' to the specified time range.
#'
#' @param solution A vector representing a candidate solution for the association rule.
#' @param features A list containing information about features, including type and bounds.
#' @param instances A data frame representing instances in the dataset.
#' @param is_time_series A boolean flag indicating if time series filtering is required.
#'
#' @return The fitness of the association rule and identified rule.
#'
#' @export
evaluate <- function(solution, features, instances, is_time_series = FALSE) {
  total_transactions <- nrow(instances)

  filtered_instances <- instances

  # Process time series bounds if applicable
  if (is_time_series) {
    start_time <- abs(tail(solution, 2)[1])
    end_time <- abs(tail(solution, 2)[2])
    solution <- head(solution, -2)

    time_bounds <- map_to_ts(start_time, end_time, instances)
    filtered_instances <- time_bounds$filtered_instances
  }

  # Build rule from solution
  rule <- build_rule(solution, features)
  fitness <- -1.0
  support_conf <- NULL

  if (length(rule) > 1) {
    # Extract antecedent and consequent using cut point
    cut <- cut_point(abs(solution[length(solution)]), length(rule))
    antecedent <- rule[1:cut]
    consequent <- rule[(cut + 1):length(rule)]

    # Calculate support and confidence
    support_conf <- supp_conf(antecedent, consequent, filtered_instances, features)
    fitness <- calculate_fitness(support_conf$supp, support_conf$conf)

    # Add support, confidence, and fitness to rule
    rule <- list(
      antecedent = antecedent,
      consequent = consequent,
      support = support_conf$supp,
      confidence = support_conf$conf,
      fitness = fitness
    )
  }

  return(list(fitness = fitness, rules = list(rule)))
}

#' Map solution boundaries to time series indices.
#'
#' This function maps the lower and upper bounds of the solution vector to
#' transaction indices based on the length of the time series.
#'
#' @param lower The lower bound of the time range in [0, 1].
#' @param upper The upper bound of the time range in [0, 1].
#' @param total_transactions The total number of transactions in the dataset.
#'
#' @return A list with `low` and `up` indices.
#' @export
map_to_ts <- function(lower, upper, total_transactions) {
  low <- ceiling(lower * total_transactions)
  up <- floor(upper * total_transactions)  # Floor keeps the upper bound consistent

  # Ensure indices are ordered correctly
  if (low > up) {
    temp <- low
    low <- up
    up <- temp
  }

  # Ensure indices stay within bounds
  low <- max(low, 1)
  up <- min(up, total_transactions)

  return(list(low = low, up = up))
}

#' Calculate support and confidence for an association rule.
#'
#' This function calculates the support and confidence for the given antecedent
#' and consequent in the dataset instances.
#'
#' @param antecedent The antecedent part of the association rule.
#' @param consequent The consequent part of the association rule.
#' @param instances A data frame representing instances in the dataset.
#' @param features A list containing information about features, including type and bounds.
#'
#' @return A list containing support and confidence values.
#'
#' @export
supp_conf <- function(antecedent, consequent, instances, features) {
  total_transactions <- nrow(instances)

  if (total_transactions == 0) {
    return(list(supp = 0, conf = 0))
  }

  antecedent_matches <- list()
  both_matches <- list()

  # Find transactions that match !!ALL!! antecedent conditions
  for (i in 1:total_transactions) {
    row <- instances[i, , drop=FALSE]  # Ensure row remains a dataframe
    antecedent_valid <- TRUE

    for (attribute in antecedent) {
      if (!check_attribute(attribute, row)) {
        antecedent_valid <- FALSE
        break
      }
    }

    if (antecedent_valid) {
      antecedent_matches <- append(antecedent_matches, list(row))
    }
  }

  antecedent_count <- length(antecedent_matches)

  if (antecedent_count == 0) {
    return(list(supp = 0, conf = 0))
  }

  for (row in antecedent_matches) {
    consequent_valid <- TRUE

    for (attribute in consequent) {
      if (!check_attribute(attribute, row)) {
        consequent_valid <- FALSE
        break
      }
    }

    if (consequent_valid) {
      both_matches <- append(both_matches, list(row))
    }
  }

  both_count <- length(both_matches)

  supp <- round(both_count / total_transactions, digits = 10)

  conf <- if (antecedent_count > 0) {
    round(both_count / antecedent_count, digits = 10)
  } else {
    0
  }

  return(list(supp = supp, conf = conf))
}



#' Check if the attribute conditions are satisfied for an instance.
#'
#' This function checks if the attribute conditions specified in the association rule
#' are satisfied for a given instance row.
#'
#' @param attribute An attribute with type and name information.
#' @param instance_row A row representing an instance in the dataset.
#'
#' @return TRUE if conditions are satisfied, FALSE otherwise.
#'
#' @export
check_attribute <- function(attribute, instance_row) {
  attr_name <- attribute$name
  attr_type <- attribute$type

  if (!(attr_name %in% names(instance_row))) {
    print(paste("Missing attribute:", attr_name))
    return(FALSE)
  }

  instance_value <- instance_row[[attr_name]]

  if (attr_type == "categorical") {
    result <- !is.na(instance_value) && as.character(instance_value) == as.character(attribute$value)
    return(result)
  } else if (attr_type == "numerical") {
    instance_value <- as.numeric(instance_value)

    if (!is.na(instance_value) && !is.null(attribute$border1) && !is.null(attribute$border2)) {
      result <- instance_value >= attribute$border1 && instance_value <= attribute$border2
    } else {
      result <- FALSE
    }
    return(result)
  }

  print(paste("Unknown attribute type:", attr_type))
  return(FALSE)
}

#' Calculate the fitness of an association rule.
#'
#' This function calculates the fitness of an association rule using support and confidence.
#'
#' @param supp The support of the association rule.
#' @param conf The confidence of the association rule.
#'
#' @return The fitness of the association rule.
#'
#' @export
calculate_fitness <- function(supp, conf) {
  return((1.0 * supp) + (1.0 * conf)) / 2
}

#' Calculate the cut point for an association rule.
#'
#' This function calculates the cut point, denoting which part of the vector belongs
#' to the antecedent and which to the consequent of the mined association rule.
#'
#' @param sol The cut value from the solution vector.
#' @param num_attr The number of attributes in the association rule.
#'
#' @return The cut point value.
#'
#' @export
cut_point <- function(sol, num_attr) {
  cut <- trunc(sol * num_attr)
  cut <- ifelse(cut == 0, 1, cut)
  cut <- ifelse(cut > (num_attr - 1), num_attr - 2, cut)

  return(cut)
}
