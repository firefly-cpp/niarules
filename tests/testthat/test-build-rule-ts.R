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

# obtain cut point value
cut_value <- abs(tail(solution, 1))
all.equal(cut_value, 0.60543971)

# calculate cut point value
cut <- cut_point(cut_value, length(features))
expect_equal(cut, 3)

solution <- head(solution, -1)

# test start and end interval
result <- map_to_ts(lower = 0.13133695, upper = 0.23055155, instances = data)
expect_equal(result$low, 14)
expect_equal(result$up, 23)

expect_equal(data$timestamp[result$low], "2024-09-08 20:16:21")
expect_equal(data$timestamp[result$up], "2024-09-08 20:17:51")

filtered_instances <- result$filtered_instances

### TEST METRICS ###
# SCENARIO 1
ant <- list()
ant <- add_attribute(ant, "weather", "categorical", 1, 1, "clouds")

con <- list()
con <- add_attribute(con, "temperature", "numerical", 28.5, 28.5, "A")

metrics <- supp_conf(ant, con, filtered_instances, features)
expect_equal(metrics$supp, 0.2)
expect_equal(metrics$conf, 0.2)

# SCENARIO 2
ant2 <- list()
ant2 <- add_attribute(ant2, "weather", "categorical", 1, 1, "clouds")
ant2 <- add_attribute(ant2, "humidity", "numerical", 60.23, 65.8921, "A")

con2 <- list()
con2 <- add_attribute(con2, "temperature", "numerical", 0, 100, "A")

metrics2 <- supp_conf(ant2, con, filtered_instances, features)
expect_equal(metrics2$supp, 0.0)
expect_equal(metrics2$conf, 0.0)

# SCENARIO 3
ant3 <- list()
ant3 <- add_attribute(ant3, "weather", "categorical", 1, 1, "clouds")
ant3 <- add_attribute(ant3, "humidity", "numerical", 60.23, 65.8921, "A")
ant3 <- add_attribute(ant3, "light", "numerical", 13.00, 20.8921, "A")

metrics3 <- supp_conf(ant3, con, filtered_instances, features)
expect_equal(metrics3$supp, 0.0)
expect_equal(metrics3$conf, 0.0)

# COMBINATIONS

metrics4 <- supp_conf(ant, con2, filtered_instances, features)
expect_equal(metrics4$supp, 1.0)
expect_equal(metrics4$conf, 1.0)

metrics5 <- supp_conf(ant2, con2, filtered_instances, features)
expect_equal(metrics5$supp, 0.3)
expect_equal(metrics5$conf, 1.0)

metrics6 <- supp_conf(ant3, con2, filtered_instances, features)
expect_equal(metrics6$supp, 0.1)
expect_equal(metrics6$conf, 1.0)
