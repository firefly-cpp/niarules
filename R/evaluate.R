#' Evaluate a candidate solution.
#'
#' This function takes a canditate solution (vector), list of features, and instances, and evaluates the fitness
#' of an association rule by calculating support and confidence.
#'
#' @param solution A vector representing a candidate solution for the association rule.
#' @param features A list containing information about features, including type and bounds.
#' @param instances A data frame representing instances in the dataset.
#'
#' @return The fitness of the association rule and identified rule.
#'
#' @export
evaluate <- function(solution, features, instances) {
  # Obtain cut point value and remove this value from a vector of solutions
  # TODO:: abs
  cut_value <- abs(tail(solution, 1))
  solution <- head(solution, -1)

  # Build a rule from the candidate solution
  rule <- build_rule(solution, features)

  # Initialize fitness
  fitness <- -1.0

  if (length(rule) > 1)
  {
    # Calculate cut point
    cut <- cut_point(cut_value, length(rule))

    # Get antecedent and consequent of the rule
    antecedent <- rule[1:cut]
    consequent <- rule[(cut + 1):length(rule)]

  if (length(antecedent) > 0 && length(consequent) > 0) {
    support_conf <- supp_conf(antecedent, consequent, instances, features)
    fitness <- calculate_fitness(support_conf$supp, support_conf$conf)
  }

  rule <- list()
  # save association rule if fitness is greater than 0
  if (fitness > 0.0)
  {
    rule <- add_association_rule(rule, antecedent, consequent, support_conf$supp, support_conf$conf, fitness)
  }
  }

return(list(fitness=fitness, rules=rule))
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
    numant <- sum(sapply(antecedent, function(attribute)
      check_attribute(attribute, instances[i, ])))
    if (numant == length(antecedent)) {
      ant_final <- ant_final + 1

      numcon <- sum(sapply(consequent, function(attribute)
        check_attribute(attribute, instances[i, ])))

      if (numcon == length(consequent)) {
        con_final <- con_final + 1
      }
    }
  }

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
  if (attribute$type != "categorical") {
    feature_min <- attribute$border1
    feature_max <- attribute$border2
    return(instance_row[attribute$name] >= feature_min &&
             instance_row[attribute$name] <= feature_max)
  } else {
    return(attribute$value == instance_row[attribute$name])
  }
}

#' Get the lower and upper bounds of a feature.
#'
#' This function retrieves the lower and upper bounds of a feature from the features list.
#'
#' @param features A list containing information about features, including type and bounds.
#' @param name The name of the feature.
#'
#' @return A list containing the lower and upper bounds of the feature.
#'
#' @export
feature_borders <- function(features, name) {
  min_val <- 0.0
  max_val <- 0.0

  feat_names <- names(features)
  for (f in feat_names) {
    if (f == name) {
      min_val <- features[[f]]$lower_bound
      max_val <- features[[f]]$upper_bound
      break
    }
  }

  return(list(feature_min = min_val, feature_max = max_val))
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
  #TODO allow to specify alpha, beta, etc. as a parameter.
  return ((1.0 * supp) + (1.0 * conf)) / 2
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
  # Calculate cut point.
  cut <- trunc(sol * num_attr)
  cut <- ifelse(cut == 0, 1, cut)
  cut <- ifelse(cut > (num_attr - 1), num_attr - 2, cut)

  return(cut)
}

#' Add an association rule to the list of rules.
#'
#' This function adds a new association rule to the existing list of rules.
#'
#' @param rules The current list of association rules.
#' @param antecedent The antecedent part of the association rule.
#' @param consequence The consequent part of the association rule.
#' @param support The support of the association rule.
#' @param confidence The confidence of the association rule.
#' @param fitness The fitness of the association rule.
#'
#' @return The updated list of association rules.
#'
#' @export
add_association_rule <- function(rules, antecedent, consequence,
                                 support, confidence, fitness) {
  new_rule <- list(
    antecedent = antecedent,
    consequence = consequence,
    support = support,
    confidence = confidence,
    fitness = fitness
  )
  return(c(rules, list(new_rule)))
}
