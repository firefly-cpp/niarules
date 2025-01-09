library(niarules)
library(testthat)

# Set a tolerance level
tolerance <- 1e-8

# time series dataset
data <- read_dataset("datasets/ts2.csv", timestamp_col = "timestamp")

num_instances <- nrow(data)
expect_equal(num_instances, 100)

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

solution <- c(
  0.93186346, 0.62861471, 0.34720034, 0.51736529, 0.34957089, 0.06362278,
  0.52224747, 0.31581756, 0.78154328, 0.60825901, 0.81263313, 0.4070408,
  0.93014498, 0.46848055, 0.15840165, 0.14308865, 0.86379166, 0.46777855,
  0.60746777, 0.13133695, 0.23055155, 0.60543971
)

expect_equal(length(solution), 22)

# obtain cut value
cut_value <- abs(tail(solution, 1))
all.equal(cut_value, 0.60543971)

# calculate cut point
cut <- cut_point(cut_value, length(features))
expect_equal(cut, 3)

solution <- head(solution, -1)

# check metrics
antecedents <- list()
antecedents <- add_attribute(antecedents, "weather", "categorical", 1, 1, "clouds")

consequence <- list()
consequence <- add_attribute(consequence, "temperature", "numerical", 28.5, 28.5, "A")

# test start and end interval
result <- map_to_ts(lower = 0.13133695, upper = 0.23055155, total_transactions = num_instances)
expect_equal(result$low, 13)
expect_equal(result$up, 23)

expect_equal(data$timestamp[13], "2024-09-08 20:16:11")
expect_equal(data$timestamp[23], "2024-09-08 20:17:51")

filtered_instances <- data[result$low:result$up, ]

metrics <- supp_conf(antecedents, consequence, filtered_instances, features)
expect_equal(metrics$supp, 0.272727272727)
expect_equal(metrics$conf, 0.272727272727)

# SCENARIO 2
antecedents2 <- list()
antecedents2 <- add_attribute(antecedents, "weather", "categorical", 1, 1, "clouds")
antecedents2 <- add_attribute(antecedents, "humidity", "numerical", 60.23, 65.8921, "A")

consequence2 <- list()
consequence2 <- add_attribute(consequence, "temperature", "numerical", 0, 100, "A")

metrics2 <- supp_conf(antecedents2, consequence2, filtered_instances, features)
expect_equal(metrics2$supp, 0.3)
expect_equal(metrics2$conf, 1.0)

# SCENARIO 3
antecedents3 <- list()
antecedents3 <- add_attribute(antecedents, "weather", "categorical", 1, 1, "clouds")
antecedents3 <- add_attribute(antecedents, "humidity", "numerical", 60.23, 65.8921, "A")
antecedents3 <- add_attribute(antecedents, "light", "numerical", 13.00, 20.8921, "A")

metrics3 <- supp_conf(antecedents3, consequence2, filtered_instances, features)
expect_equal(metrics3$supp, 0.1)
expect_equal(metrics3$conf, 1.0)
