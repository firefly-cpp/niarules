% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{write_association_rules_to_csv}
\alias{write_association_rules_to_csv}
\title{Write Association Rules to CSV file}
\usage{
write_association_rules_to_csv(
  rules,
  file_path,
  is_time_series = FALSE,
  timestamps = NULL
)
}
\arguments{
\item{rules}{A list of association rules.}

\item{file_path}{The file path for the CSV output.}

\item{is_time_series}{A boolean flag indicating if time series information should be included.}

\item{timestamps}{A vector of timestamps corresponding to the time series data.}
}
\value{
No explicit return value. The function writes association rules to a CSV file.
}
\description{
This function writes association rules to a CSV file. For time series datasets,
it also includes start and end timestamps instead of indices.
}
