library("niarules")
library("testthat")

context("Metrics tests")

dataset <- "datasets/wiki_test_case.csv"

# read dataset
data <- read_dataset(dataset)

# get features
features = extract_feature_info(data)

antecedents <- list()
antecedents <- add_rule(antecedents, "Feat1", "categorical", 0.2, 0.8, "A")

consequence <- list()
consequence <- add_rule(consequence, "Feat2", "numerical", 1, 1, "EMPTY")

# TODO
metrics <- supp_conf(antecedents, consequence, data, features)

