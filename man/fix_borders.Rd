% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/de.R
\name{fix_borders}
\alias{fix_borders}
\title{Fix Borders of a Numeric Vector}
\usage{
fix_borders(vector)
}
\arguments{
\item{vector}{A numeric vector to be processed.}
}
\value{
A numeric vector with borders fixed.
}
\description{
This function ensures that all values greater than 1.0 are set to 1.0,
and all values less than 0.0 are set to 0.0.
}
