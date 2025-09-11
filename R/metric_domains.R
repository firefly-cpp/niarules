#' Compute metric domains (experimental)
#' @description `r lifecycle::badge('experimental')`
#'
#' Returns numeric ranges [min, max] for the metrics `support`, `confidence`,
#' and `lift` from various input types.
#'
#' @param x An object containing rule metrics. Methods are provided for
#'   objects of class `parsed`, and for a `data.frame`.
#'   (Optionally also for class `niarules_edges` if you tag it.)
#' @param ... Passed to methods (currently unused).
#'
#' @return A named list with numeric vectors of length 2:
#'   `list(support = c(min, max), confidence = c(min, max), lift = c(min, max))`.
#'
#' @export
#' @rdname metric_domains
metric_domains <- function(x, ...) UseMethod("metric_domains")

# helper used by all methods
.rng_ok <- function(v, fallback = c(0, 1)) {
  r <- suppressWarnings(range(v, na.rm = TRUE, finite = TRUE))
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[2] < r[1]) fallback else r
}

#' @export
#' @rdname metric_domains
metric_domains.parsed <- function(x, ...) {
  r <- x$rules
  
  col <- function(cands) {
    nms <- names(r); i <- match(tolower(cands), tolower(nms)); i <- i[!is.na(i)]
    if (length(i) == 0) stop("metric_domains(parsed): none of columns {", paste(cands, collapse=", "), "} found")
    r[[ nms[i[1]] ]]
  }
  
  list(
    support    = .rng_ok(col(c("support"))),
    confidence = .rng_ok(col(c("confidence"))),
    lift       = .rng_ok(col(c("lift","fitness","Fitness")))
  )
}

#' @export
#' @rdname metric_domains
metric_domains.data.frame <- function(x, ...) {
  findcol <- function(target) {
    i <- which(tolower(names(x)) == target)
    if (length(i) < 1) stop("metric_domains(data.frame): column '", target, "' not found")
    names(x)[i[1]]
  }
  cs <- findcol("support")
  cc <- findcol("confidence")
  cl <- {
    j <- which(tolower(names(x)) %in% c("lift","fitness"))
    if (length(j) < 1) stop("metric_domains(data.frame): need 'lift' (or 'Fitness')")
    names(x)[j[1]]
  }
  
  list(
    support    = .rng_ok(x[[cs]]),
    confidence = .rng_ok(x[[cc]]),
    lift       = .rng_ok(x[[cl]])
  )
}

#' @export
#' @rdname metric_domains
metric_domains.default <- function(x, ...) {
  stop("metric_domains(): no method for class(es) ", paste(class(x), collapse = "/"))
}