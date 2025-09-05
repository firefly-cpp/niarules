#' @export
metric_domains <- function(x, ...) UseMethod("metric_domains")

#' @export
metric_domains.parsed <- function(parsed, ...) {
  r <- parsed$rules
  list(
    support    = range(r$support,    finite = TRUE),
    confidence = range(r$confidence, finite = TRUE),
    lift       = range(r$lift,       finite = TRUE)
  )
}

#' @export
metric_domains.edges <- function(edges, ...) {
  list(
    support    = range(edges$support,    finite = TRUE),
    confidence = range(edges$confidence, finite = TRUE),
    lift       = range(edges$lift,       finite = TRUE)
  )
}

#' @export
metric_domains.data.frame <- function(edges, ...) {
  # Case-insensitive column lookup
  findcol <- function(target) {
    i <- which(tolower(names(edges)) == target)
    if (length(i) < 1) stop("metric_domains(): column '", target, "' not found in data.frame")
    names(edges)[i[1]]
  }
  cs <- findcol("support")
  cc <- findcol("confidence")
  cl <- findcol("lift")
  
  rng_ok <- function(x) {
    r <- range(x, finite = TRUE)
    if (!is.finite(r[1]) || !(r[2] > r[1])) c(0, 1) else r
  }
  
  list(
    support    = rng_ok(edges[[cs]]),
    confidence = rng_ok(edges[[cc]]),
    lift       = rng_ok(edges[[cl]])
  )
}