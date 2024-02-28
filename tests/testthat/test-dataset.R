library("niarules")
library("testthat")

context("Dataset tests")

dataset <- "datasets/Abalone.csv"

# read dataset
data <- read_dataset(dataset)

# get features
features = extract_feature_info(data)

# basic tests
expect_equal(length(features), 9)
expect_equal(features[[4]]$type, "numerical")
expect_equal(features[[4]]$lower_bound, 0)
expect_equal(features[[4]]$upper_bound, 1.13)
expect_equal(names(features)[1], "Sex")
expect_equal(names(features)[5], "Whole.weight")

# dimension of the problem
dim <- problem_dimension(features)
expect_equal(dim, 36)
