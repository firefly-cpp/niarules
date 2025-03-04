#' Evaluate a candidate solution, with optional time series filtering.
#'
#' This function evaluates the fitness of an association rule using support and confidence.
#' If time series data is used, it restricts evaluation to the specified time range.
#'
#' @param solution A vector representing a candidate solution.
#' @param features A list containing information about features.
#' @param instances A data frame representing dataset instances.
#' @param is_time_series A boolean flag indicating if time series filtering is required.
#'
#' @return A list containing fitness and identified rules.
#'
#' @references
#' Fister, I., Iglesias, A., Galvez, A., Del Ser, J., Osaba, E., & Fister, I. (2018).
#' "Differential evolution for association rule mining using categorical and numerical attributes."
#' In Intelligent Data Engineering and Automated Learning–IDEAL 2018: 19th International Conference,
#' Madrid, Spain, November 21–23, 2018, Proceedings, Part I (pp. 79-88). Springer International Publishing.
#' \doi{10.1007/978-3-030-03496-2_9}
#'
#' Fister Jr, I., Podgorelec, V., & Fister, I. (2021). "Improved nature-inspired algorithms for numeric
#' association rule mining." In Intelligent Computing and Optimization: Proceedings of the 3rd International
#' Conference on Intelligent Computing and Optimization 2020 (ICO 2020) (pp. 187-195).
#' Springer International Publishing. \doi{10.1007/978-3-030-68154-8_19}
#'
#' @export
evaluate <- function(solution, features, instances, is_time_series = FALSE) {
  total_transactions <- nrow(instances)
  
  # Process time series filtering if applicable
  if (is_time_series) {
    start_time <- abs(tail(solution, 2)[1])
    end_time <- abs(tail(solution, 2)[2])
    solution <- head(solution, -2)  # Remove time bounds from solution
    
    time_bounds <- map_to_ts(start_time, end_time, instances)
    instances <- time_bounds$filtered_instances  # Filter dataset
  }
  
  # Build rule from solution
  rule <- build_rule(solution, features)
  fitness <- -1.0
  
  if (length(rule) > 1) {
    cut <- cut_point(abs(solution[length(solution)]), length(rule))
    antecedent <- rule[1:cut]
    consequent <- rule[(cut + 1):length(rule)]
    
    # Compute support and confidence only within the filtered dataset
    support_conf <- supp_conf(antecedent, consequent, instances, features)
    fitness <- calculate_fitness(support_conf$supp, support_conf$conf)
    
    # Add time range info to the rule (for output purposes)
    rule <- list(
      antecedent = antecedent,
      consequent = consequent,
      support = support_conf$supp,
      confidence = support_conf$conf,
      fitness = fitness,
      time_series_range = if (is_time_series) list(start = time_bounds$low, end = time_bounds$up) else NULL
    )
  }
  
  return(list(fitness = fitness, rules = list(rule)))
}


#' Map solution boundaries to time series instances.
#'
#' This function maps the lower and upper bounds of the solution vector to a subset of the dataset.
#'
#' @param lower The lower bound in [0, 1].
#' @param upper The upper bound in [0, 1].
#' @param instances The full dataset.
#'
#' @return A list with `low`, `up`, and `filtered_instances`.
#' @export
map_to_ts <- function(lower, upper, instances) {
  total_transactions <- nrow(instances)
  
  low <- ceiling(lower * total_transactions)
  up <- floor(upper * total_transactions)
  
  # Swap indices if end index is lower than start index
  if (low > up) {
    temp <- low
    low <- up
    up <- temp
  }
  
  low <- max(low, 1)
  up <- min(up, total_transactions)
  
  # Extract the filtered instances
  filtered_instances <- instances[low:up, , drop = FALSE]
  
  return(list(low = low, up = up, filtered_instances = filtered_instances))
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
