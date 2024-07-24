#' Simple Random Search
#'
#' This function generates a vector of random solutions for a specified length.
#'
#' @param candidate_len The length of the vector of random solutions.
#'
#' @return A vector of random solutions between 0 and 1.
#'
#' @examples
#' candidate_len <- 10
#' random_solutions <- rs(candidate_len)
#' print(random_solutions)
#'
#' @export
rs <- function(candidate_len) {
  solutions <- runif(candidate_len)
  return(solutions)
}
