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

  time_series_range <- NULL
  filtered_instances <- instances

  # Process time series bounds if applicable
  if (is_time_series) {
    start_time <- abs(tail(solution, 2)[1])
    end_time <- abs(tail(solution, 2)[2])
    solution <- head(solution, -2)

    time_bounds <- map_to_ts(start_time, end_time, total_transactions)
    filtered_instances <- instances[time_bounds$low:time_bounds$up, ]
    time_series_range <- list(start = time_bounds$low, end = time_bounds$up)
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

  if (!is.null(time_series_range)) {
    rule$time_series_range <- time_series_range
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
#' @param total_transactions The total number of transactions in the time series.
#'
#' @return A list with `low` and `up` indices.
#' @export
map_to_ts <- function(lower, upper, total_transactions) {
  low <- trunc(total_transactions * lower)
  up <- trunc(total_transactions * upper)

  # Ensure indices are valid and ordered
  if (low > up) {
    temp <- low
    low <- up
    up <- temp
  }

  # Ensure indices are within bounds
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
  ant_final <- 0
  con_final <- 0

  for (i in 1:nrow(instances)) {
    # Check antecedent
    is_antecedent_satisfied <- all(sapply(antecedent, function(attribute) {
      check_attribute(attribute, instances[i, ])
    }))

    if (is_antecedent_satisfied) {
      ant_final <- ant_final + 1

      # Check consequent
      is_consequent_satisfied <- all(sapply(consequent, function(attribute) {
        check_attribute(attribute, instances[i, ])
      }))

      if (is_consequent_satisfied) {
        con_final <- con_final + 1
      }
    }
  }

  # Calculate support and confidence
  supp <- con_final / nrow(instances)
  conf <- ifelse(ant_final == 0, 0, con_final / ant_final)

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
  if (attribute$type == "numerical") {
    return(instance_row[attribute$name] >= attribute$border1 &&
             instance_row[attribute$name] <= attribute$border2)
  } else if (attribute$type == "categorical") {
    return(instance_row[attribute$name] == attribute$value)
  } else {
    stop("Unsupported attribute type: ", attribute$type)
  }
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
#' to the antecedent and which to the consequence of the mined association rule.
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
