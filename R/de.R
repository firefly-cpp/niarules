#' Implementation of Differential Evolution metaheuristic algorithm.
#'
#' This function uses Differential Evolution, a stochastic population-based optimization algorithm,
#' to find the optimal numerical association rule.
#'
#' @param d Dimension of the problem (default: 10).
#' @param np Population size (default: 10).
#' @param f The differential weight, controlling the amplification of the difference vector (default: 0.5).
#' @param cr The crossover probability, determining the probability of a component being replaced (default: 0.9).
#' @param nfes The maximum number of function evaluations (default: 1000).
#' @param features A list containing information about features, including type and bounds.
#' @param data A data frame representing instances in the dataset.
#' @param is_time_series A boolean indicating whether the dataset is time series.
#'
#' @return A list containing the best solution, its fitness value, and the number of function evaluations and list of identified association rules.
#'
#' @references Storn, R., & Price, K. (1997). "Differential Evolution – A Simple and Efficient Heuristic
#' for Global Optimization over Continuous Spaces." Journal of Global Optimization, 11(4), 341–359.
#' \doi{10.1023/A:1008202821328}
#'
#' @export
differential_evolution <- function(
  d = 10,
  np = 10,
  f = 0.5,
  cr = 0.9,
  nfes = 1000,
  features,
  data,
  is_time_series = FALSE
) {
  # A list for storing all association rules
  arules <- list()

  # Initialize population
  population <- matrix(runif(np * d, 0.0, 1.0), nrow = np, ncol = d)

  # Evaluate the objective function for each individual in the population
  results <- apply(
    population,
    1,
    function(individual) evaluate(individual, features, data, is_time_series)
  )

  # Extract fitness values and association rules separately
  fitness_values <- sapply(results, function(result) result$fitness)
  arule <- lapply(results, function(result) result$rules)

  # Save identified rules
  filter_rules <- arule[sapply(arule, function(r) is.list(r) && !is.null(r$fitness))]
  if (length(filter_rules) > 0) {
    arules <- c(arules, unlist(filter_rules, recursive = FALSE))
  }

  # Start counting function evaluations
  num_evaluations <- np

  # Main optimization loop
  while (num_evaluations < nfes) {
    # Create new population
    new_population <- matrix(NA, nrow = np, ncol = d)

    for (i in 1:np) {
      # Select three distinct individuals
      candidates <- sample(1:np, 3, replace = FALSE)

      # Apply differential mutation
      mutant_vector <- population[candidates[1], ] +
        f * (population[candidates[2], ] - population[candidates[3], ])

      # Apply differential crossover
      crossover_mask <- runif(d) < cr
      trial_vector <- ifelse(crossover_mask, mutant_vector, population[i, ])

      # Ensure values are within [0, 1]
      trial_vector <- fix_borders(trial_vector)

      # Evaluate the trial vector
      fit <- evaluate(trial_vector, features, data, is_time_series)
      trial_fitness <- fit$fitness

      # Save rule if fitness is greater than 0
      if (trial_fitness > 0.0) {
        valid_rules <- Filter(function(r) is.list(r) && !is.null(r$fitness), fit$rules)
        arules <- c(arules, valid_rules)
      }

      # Increment the number of function evaluations
      num_evaluations <- num_evaluations + 1

      # Select the better vector
      if (trial_fitness > fitness_values[i]) {
        new_population[i, ] <- trial_vector
        fitness_values[i] <- trial_fitness
      } else {
        new_population[i, ] <- population[i, ]
      }

      # Break if maximum evaluations reached
      if (num_evaluations >= nfes) {
        break
      }
    }

    # Update the population
    population <- new_population

    # Check if maximum evaluations reached
    if (num_evaluations >= nfes) {
      break
    }
  }

  # Find the best individual
  best_index <- which.max(fitness_values)
  best_solution <- population[best_index, ]
  best_fitness <- fitness_values[best_index]

  # Remove empty or invalid rules
  arules <- arules[sapply(arules, function(r) is.list(r) && !is.null(r$fitness))]

  return(list(
    best_solution = best_solution,
    best_fitness = best_fitness,
    num_evaluations = num_evaluations,
    arules = arules
  ))
}

#' Fix Borders of a Numeric Vector
#'
#' This function ensures that all values greater than 1.0 are set to 1.0,
#' and all values less than 0.0 are set to 0.0.
#'
#' @param vector A numeric vector to be processed.
#'
#' @return A numeric vector with borders fixed.
#' @export
fix_borders <- function(vector) {
  vector[vector > 1.0] <- 1.0
  vector[vector < 0.0] <- 0.0
  return(vector)
}
