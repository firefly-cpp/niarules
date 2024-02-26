library("niarules")
library("testthat")

context("Metrics tests")

dataset <- "datasets/Abalone.csv"

# read dataset
data <- read_dataset(dataset)

# get features
features = extract_feature_info(data)

antecedents <- list()
antecedents <- add_attribute(antecedents, "Height", "numerical", 0.015, 0.44, "A")
antecedents <- add_attribute(antecedents, "Rings", "numerical", 3, 15, "A")

consequence <- list()
consequence <- add_attribute(consequence, "Sex", "categorical", 1, 1, "I")

metrics <- supp_conf(antecedents, consequence, data, features)
all.equal(metrics$supp, 0.3145798)
all.equal(metrics$conf, 0.3360614)

