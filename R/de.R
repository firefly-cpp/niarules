#' Implementation of Differential Evolution metaheuristic algorithm.
#'
#' This function uses Differential Evolution, a stochastic population-based optimization algorithm,
#' to find the optimal numerical association rule.
#'
#' @param D Dimension of the problem (default: 10).
#' @param NP Population size (default: 10).
#' @param F The differential weight, controlling the amplification of the difference vector (default: 0.5).
#' @param CR The crossover probability, determining the probability of a component being replaced (default: 0.9).
#' @param nfes The maximum number of function evaluations (default: 1000).
#' @param features A list containing information about features, including type and bounds.
#' @param data A data frame representing instances in the dataset.
#'
#' @return A list containing the best solution, its fitness value, and the number of function evaluations and list of identified association rules.
#'
#' @export
differential_evolution <- function(D = 10, NP = 10, F = 0.5, CR = 0.9, nfes = 1000, features, data) {
  # a list for storing all association rules
  arules <- list()

  # Initialize population
  population <- matrix(runif(NP * D, 0.0, 1.0), nrow = NP, ncol = D)

  # Evaluate the objective function for each individual in the population
  results <- apply(population, 1, function(individual) evaluate(individual, features, data))

  # Extract fitness values and association rules separately
  fitness_values <- sapply(results, function(result) result$fitness)
  arule <- lapply(results, function(result) result$rules)

  # Save identified rule
  # Remove empty lists
  filter_rules <- arule[sapply(arule, length) > 0]

  if (length(filter_rules) > 0)
  {
   arules <- c(arules, filter_rules)
  }

  # Start counting function evaluations
  num_evaluations <- NP

  # Main optimization loop based on function evaluations
  while (num_evaluations < nfes) {
    # Create new population
    new_population <- matrix(NA, nrow = NP, ncol = D)

    for (i in 1:NP) {
      # Select three distinct individuals
      candidates <- sample(1:NP, 3, replace = FALSE)

      # Apply diferential mutation
      mutant_vector <- population[candidates[1], ] + F *
        (population[candidates[2], ] - population[candidates[3], ])

      # Apply differential crossover
      crossover_mask <- runif(D) < CR
      trial_vector <- ifelse(crossover_mask, mutant_vector,
                             population[i, ])

      # check for bounds (should be between 0 and 1)
      trial_vector <- fixBorders(trial_vector)
      # Evaluate the objective function
      fit <- evaluate(trial_vector, features, data)
      trial_fitness <- fit$fitness

      # save rule if fitness greater than 0
      if (trial_fitness > 0.0)
      {
       arules <- c(arules, fit$rule)
      }

      # Increment the number of function evaluations
      num_evaluations <- num_evaluations + 1

      # Select the better vector between the trial and the current individual
      if (trial_fitness > fitness_values[i]) {
        new_population[i, ] <- trial_vector
        fitness_values[i] <- trial_fitness
      } else {
        new_population[i, ] <- population[i, ]
      }

      # Check if the maximum number of evaluations is reached
      if (num_evaluations >= nfes) {
        break
      }
    }

    # Update the population
    population <- new_population

    # Check if the maximum number of evaluations is reached
    if (num_evaluations >= nfes) {
      break
    }
  }

  # Find the best individual in the final population
  best_index <- which.max(fitness_values)
  best_solution <- population[best_index, ]
  best_fitness <- fitness_values[best_index]

  # Remove empty list in the nested list
  arules <- arules[sapply(arules, length) > 1]

  return(list("best_solution" = best_solution, "best_fitness" =
                best_fitness, "num_evaluations" = num_evaluations, "arules" = arules))
}

#' Fix Borders of a Numeric Vector
#'
#' This function takes a numeric vector as input and ensures that all values
#' greater than 1.0 are set to 1.0, and all values less than 0.0 are set to 0.0.
#'
#' @param vector A numeric vector to be processed.
#'
#' @return A numeric vector with borders fixed. Values greater than 1.0 are
#'   replaced with 1.0, and values less than 0.0 are replaced with 0.0.
#'
#' @examples
#' original_vector <- c(1.19007417, 0.33135271, -0.5, 1.5, 0.0)
#' fixed_vector <- fixBorders(original_vector)
#' print(fixed_vector)
#'
#' @export
fixBorders <- function(vector) {
  vector[vector > 1.0] <- 1.0
  vector[vector < 0.0] <- 0.0
  return(vector)
}

