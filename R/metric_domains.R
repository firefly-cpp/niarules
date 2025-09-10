#' Compute domains (ranges) of rule metrics
#'
#' Returns numeric ranges \[min, max\] for the metrics `support`, `confidence`,
#' and `lift` from various input types.
#'
#' @param x An object containing rule metrics. Methods are provided for
#'   objects of class `parsed`, `edges`, and for a `data.frame`.
#' @param ... Passed to methods (currently unused).
#'
#' @return A named list with numeric vectors of length 2:
#'   `list(support = c(min, max), confidence = c(min, max), lift = c(min, max))`.
#'
#' @export
#' @rdname metric_domains
metric_domains <- function(x, ...) UseMethod("metric_domains")

#' @export
#' @rdname metric_domains
metric_domains.parsed <- function(x, ...) {
  r <- x$rules
  list(
    support    = range(r$support,    finite = TRUE),
    confidence = range(r$confidence, finite = TRUE),
    lift       = range(r$lift,       finite = TRUE)
  )
}

#' @export
#' @rdname metric_domains
metric_domains.edges <- function(x, ...) {
  list(
    support    = range(x$support,    finite = TRUE),
    confidence = range(x$confidence, finite = TRUE),
    lift       = range(x$lift,       finite = TRUE)
  )
}

#' @export
#' @rdname metric_domains
metric_domains.data.frame <- function(x, ...) {
  # Case-insensitive column lookup
  findcol <- function(target) {
    i <- which(tolower(names(x)) == target)
    if (length(i) < 1) stop("metric_domains(): column '", target, "' not found in data.frame")
    names(x)[i[1]]
  }
  cs <- findcol("support")
  cc <- findcol("confidence")
  cl <- findcol("lift")
  
  rng_ok <- function(v) {
    r <- range(v, finite = TRUE)
    if (!is.finite(r[1]) || !(r[2] > r[1])) c(0, 1) else r
  }
  
  list(
    support    = rng_ok(x[[cs]]),
    confidence = rng_ok(x[[cc]]),
    lift       = rng_ok(x[[cl]])
  )
}