library(niarules)
library(testthat)

# Set a tolerance level
tolerance <- 1e-4

# time series dataset
data <- read_dataset("datasets/ts2.csv", timestamp_col = "timestamp")

# get features
features <- extract_feature_info(data)

# basic tests for time series dataset
expect_equal(length(features), 5)
expect_equal(features[[1]]$type, "numerical")
expect_equal(features[[4]]$type, "numerical")
expect_equal(features[[5]]$type, "categorical")
expect_equal(features[[4]]$lower_bound, 0)

# dimension of the problem
dim <- problem_dimension(features, is_time_series = TRUE)
expect_equal(dim, 22)

