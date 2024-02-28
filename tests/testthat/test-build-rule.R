library(niarules)
library(testthat)

dataset <- "datasets/Abalone.csv"
data <- read_dataset(dataset)
features <- extract_feature_info(data)

solution <- c(0.80254630, 0.82842399, 0.13943825, 0.32911782, 0.80899805, 0.27442505, 0.13613248, 0.10763776,
               0.17800705, 0.12497644, 0.64532122, 0.14505979, 0.13384908, 0.34350675, 0.22888480, 0.38928681,
               0.67350952, 0.58634687, 0.05497724, 0.45963655, 0.92223457, 0.22361950, 0.26751035, 0.55755924,
               0.66104474, 0.14780222, 0.89737463, 0.19895011, 0.89795090, 0.82693416, 0.17140162, 0.25130162,
               0.77725946, 0.15132663, 0.97976923, 0.67946213)

# Extract permutation
permutation <- solution[(length(solution) - length(features) + 1):length(solution)]

# Test vector positions
vector_position <- function(feature_name) {
  feature_position(features, feature_name)
}

expect_equal(length(solution), 36)
all.equal(permutation, c(0.1989501, 0.8979509, 0.8269342, 0.1714016, 0.2513016, 0.7772595, 0.1513266, 0.9797692, 0.6794621))

real_solution <- c(0.80254630, 0.82842399, 0.13943825, 0.32911782, 0.80899805, 0.27442505, 0.13613248, 0.10763776,
                 0.17800705, 0.12497644, 0.64532122, 0.14505979, 0.13384908, 0.34350675, 0.22888480, 0.38928681,
                 0.67350952, 0.58634687, 0.05497724, 0.45963655, 0.92223457, 0.22361950, 0.26751035, 0.55755924,
                 0.66104474, 0.14780222, 0.89737463)

# Remove permutation from solution
solution <- solution[-((length(solution) - length(features) + 1):length(solution))]
expect_equal(solution, real_solution)

# Order permutation
permutation2 <- order(permutation, decreasing = TRUE)
expect_equal(permutation2, c(8, 2, 3, 6, 9, 5, 1, 4, 7))

# Test vector positions
expect_equal(vector_position("Length"), 3)
expect_equal(vector_position("Diameter"), 5)
expect_equal(vector_position("Rings"), 17)
