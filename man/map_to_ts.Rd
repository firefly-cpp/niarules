% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.R
\name{map_to_ts}
\alias{map_to_ts}
\title{Map solution boundaries to time series instances.}
\usage{
map_to_ts(lower, upper, instances)
}
\arguments{
\item{lower}{The lower bound in [0, 1].}

\item{upper}{The upper bound in [0, 1].}

\item{instances}{The full dataset.}
}
\value{
A list with `low`, `up`, and `filtered_instances`.
}
\description{
This function maps the lower and upper bounds of the solution vector to a subset of the dataset.
}
