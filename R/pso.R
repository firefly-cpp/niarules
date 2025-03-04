#' Implementation of Particle Swarm Optimization (PSO) metaheuristic algorithm.
#'
#' This function uses PSO, a stochastic population-based optimization algorithm,
#' to find the optimal numerical association rule.
#'
#' @param d Dimension of the problem (default: 10).
#' @param np Population size (default: 10).
#' @param w Inertia weight (default: 0.7).
#' @param c1 Cognitive coefficient (default: 1.5).
#' @param c2 Social coefficient (default: 1.5).
#' @param nfes The maximum number of function evaluations (default: 1000).
#' @param features A list containing information about features, including type and bounds.
#' @param data A data frame representing instances in the dataset.
#' @param is_time_series A boolean indicating whether the dataset is time series.
#'
#' @return A list containing the best solution, its fitness value, and the number of function evaluations and list of identified association rules.
#'
#' @references Kennedy, J., & Eberhart, R. (1995). "Particle swarm optimization."
#' Proceedings of ICNN'95 - International Conference on Neural Networks, 4, 1942–1948.
#' IEEE. \doi{10.1109/ICNN.1995.488968}
#'
#' @export
particle_swarm_optimization <- function(
  d = 10,
  np = 10,
  w = 0.7,
  c1 = 1.5,
  c2 = 1.5,
  nfes = 1000,
  features,
  data,
  is_time_series = FALSE
) {
  # A list for storing all association rules
  arules <- list()

  # Initialize population and velocity
  population <- matrix(runif(np * d, 0.0, 1.0), nrow = np, ncol = d)
  velocity <- matrix(runif(np * d, -0.5, 0.5), nrow = np, ncol = d)

  # Evaluate fitness
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

  # Initialize personal best and global best
  pbest <- population
  pbest_fitness <- fitness_values
  gbest_index <- which.max(fitness_values)
  gbest <- population[gbest_index, ]
  gbest_fitness <- fitness_values[gbest_index]

  # Start counting function evaluations
  num_evaluations <- np

  # Main optimization loop
  while (num_evaluations < nfes) {
    for (i in 1:np) {
      # Update velocity
      r1 <- runif(d)
      r2 <- runif(d)
      velocity[i, ] <- w * velocity[i, ] +
        c1 * r1 * (pbest[i, ] - population[i, ]) +
        c2 * r2 * (gbest - population[i, ])

      # Update position
      population[i, ] <- population[i, ] + velocity[i, ]

      # Ensure values are within [0, 1]
      population[i, ] <- fix_borders(population[i, ])

      # Evaluate new candidate
      fit <- evaluate(population[i, ], features, data, is_time_series)
      trial_fitness <- fit$fitness

      # Save rule if fitness is greater than 0
      if (trial_fitness > 0.0) {
        valid_rules <- Filter(function(r) is.list(r) && !is.null(r$fitness), fit$rules)
        arules <- c(arules, valid_rules)
      }

      # Increment evaluations
      num_evaluations <- num_evaluations + 1

      # Update personal best
      if (trial_fitness > pbest_fitness[i]) {
        pbest[i, ] <- population[i, ]
        pbest_fitness[i] <- trial_fitness
      }

      # Update global best
      if (trial_fitness > gbest_fitness) {
        gbest <- population[i, ]
        gbest_fitness <- trial_fitness
      }

      # Break if max evaluations reached
      if (num_evaluations >= nfes) {
        break
      }
    }
  }

  # Remove empty or invalid rules
  arules <- arules[sapply(arules, function(r) is.list(r) && !is.null(r$fitness))]

  return(list(
    best_solution = gbest,
    best_fitness = gbest_fitness,
    num_evaluations = num_evaluations,
    arules = arules
  ))
}
