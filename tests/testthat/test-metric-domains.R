library(testthat)

test_that("metric_domains works for parsed objects", {
  df <- data.frame(
    Antecedent = "A = a", Consequence = "Y = y",
    support = 0.2, Confidence = 0.4, lift = 1.5,
    stringsAsFactors = FALSE
  )
  p <- parse_rules(df)
  d <- metric_domains(p)
  expect_equal(sort(names(d)), c("confidence","lift","support"))
  expect_length(d$support, 2); expect_true(d$support[1] <= d$support[2])
})

test_that("metric_domains works for data.frame (case-insensitive, Fitness alias)", {
  edges <- data.frame(
    support = c(0.1, 0.3), Confidence = c(0.2, 0.5), Fitness = c(1.2, 2.0)
  )
  d <- metric_domains(edges)
  expect_identical(d$lift, range(edges$Fitness, finite = TRUE))
})
