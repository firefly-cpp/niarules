library(testthat)

# helper to build a minimal niarules-like data.frame
mk_rules_df <- function() {
  data.frame(
    Antecedent = c(
      'age in [18, 35] & income >= 3000',
      'country = "United States", device = mobile',
      'temp in (36.5, 38.0]',
      'code = A12-7 & ratio < 0.85',
      'product = "A/B Test" & score >= 1e3'
    ),
    Consequence = c(
      "will_buy = yes",
      "{churn = no, visit = clinic}",
      "fever = yes & visit = clinic",
      "flag = true",
      "{segment = premium}"
    ),
    Support    = c(0.12, 0.08, 0.15, 0.05, 0.2),
    Confidence = c(0.6, 0.55, 0.7, 0.4, 0.75),
    Fitness    = c(1.8, 1.4, 2.1, 1.1, 2.5), # treated as lift by parse_rules_cpp
    stringsAsFactors = FALSE
  )
}

test_that("parse_rules accepts data.frame and returns expected top-level structure", {
  df <- mk_rules_df()
  parsed <- parse_rules(df)

  expect_type(parsed, "list")
  expect_true(all(c("items", "rules") %in% names(parsed)))

  items <- parsed$items
  rules <- parsed$rules

  # items data.frame columns
  expect_true(all(c(
    "item_id","label","feature","kind","category_value","lo","hi",
    "incl_low","incl_high","op","label_long","label_short"
  ) %in% names(items)))

  # rules data.frame columns
  expect_true(all(c(
    "rule_id","support","confidence","lift",
    "lhs_item_ids","rhs_item_ids","antecedent_length","consequent_length"
  ) %in% names(rules)))

  # lift equals Fitness input
  expect_equal(rules$lift, df$Fitness)

  # list-columns for lhs/rhs contain integer vectors
  expect_true(is.list(rules$lhs_item_ids))
  expect_true(is.list(rules$rhs_item_ids))
  expect_true(all(vapply(rules$lhs_item_ids, function(x) typeof(x) == "integer", logical(1))))
  expect_true(all(vapply(rules$rhs_item_ids, function(x) typeof(x) == "integer", logical(1))))

  # ids are 0-based and within range
  max_id <- max(items$item_id)
  expect_true(min(unlist(rules$lhs_item_ids)) >= 0)
  expect_true(min(unlist(rules$rhs_item_ids)) >= 0)
  expect_true(max(unlist(rules$lhs_item_ids)) <= max_id)
  expect_true(max(unlist(rules$rhs_item_ids)) <= max_id)
})

test_that("parsing splits on '&' and ',' outside brackets and trims whitespace", {
  df <- data.frame(
    Antecedent = c("A & B", "A, B", "x in (1, 2] & y = z"),
    Consequence = c("{C}", "C", "D , E"),
    Support = c(0.1, 0.1, 0.1),
    Confidence = c(0.5, 0.5, 0.5),
    Fitness = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  parsed <- parse_rules(df)
  rules <- parsed$rules

  # Rule 1: "A" and "B"
  expect_equal(length(rules$lhs_item_ids[[1]]), 2L)
  # Rule 2: also two parts, split by comma
  expect_equal(length(rules$lhs_item_ids[[2]]), 2L)
  # Rule 3: still two parts because "(1, 2]" commas are inside brackets
  expect_equal(length(rules$lhs_item_ids[[3]]), 2L)

  # RHS braces stripped and commas split
  expect_equal(length(rules$rhs_item_ids[[1]]), 1L) # "{C}" -> "C"
  expect_equal(length(rules$rhs_item_ids[[2]]), 1L) # "C"
  expect_equal(length(rules$rhs_item_ids[[3]]), 2L) # "D , E" -> "D","E"
})

test_that("registry deduplicates item strings across rules", {
  df <- data.frame(
    Antecedent = c("age in [18,35]", "age in [18,35] & country = AT"),
    Consequence = c("buy = yes", "buy = yes"),
    Support = c(0.1, 0.2),
    Confidence = c(0.6, 0.7),
    Fitness = c(1.5, 1.8),
    stringsAsFactors = FALSE
  )
  parsed <- parse_rules(df)
  items <- parsed$items
  rules <- parsed$rules

  # find the item_id for "age in [18,35]"
  age_id <- items$item_id[match("age in [18,35]", items$label)]
  expect_false(is.na(age_id))

  # it should appear in both rules' LHS
  expect_true(age_id %in% rules$lhs_item_ids[[1]])
  expect_true(age_id %in% rules$lhs_item_ids[[2]])
})

test_that("numeric intervals and relational ops are captured into metadata", {
  df <- data.frame(
    Antecedent = c("x in [1,2]", "y >= 3", 'cat = "blue"'),
    Consequence = c("Z = 1", "Z = 1", "Z = 1"),
    Support = c(0.1, 0.1, 0.1),
    Confidence = c(0.5, 0.5, 0.5),
    Fitness = c(1, 1, 1),
    stringsAsFactors = FALSE
  )
  parsed <- parse_rules(df)
  items <- parsed$items

  # interval item
  x_row <- items[items$label == "x in [1,2]", ]
  expect_equal(x_row$feature, "x")
  expect_true(is.finite(x_row$lo))
  expect_true(is.finite(x_row$hi))
  expect_true(is.logical(x_row$incl_low) && is.logical(x_row$incl_high))

  # relational item
  y_row <- items[items$label == "y >= 3", ]
  expect_equal(y_row$feature, "y")
  expect_true(is.na(y_row$hi) || isTRUE(y_row$hi == 0)) # hi typically unused in relop
  expect_equal(y_row$op, ">=")

  # categorical equality
  c_row <- items[items$label == 'cat = "blue"', ]
  expect_equal(c_row$feature, "cat")
  expect_equal(c_row$category_value, "blue")
})

test_that("scientific notation is accepted for numeric bounds", {
  df <- data.frame(
    Antecedent = c("score >= 1e3", "val in (1e-3, 2.5e-1]"),
    Consequence = c("ok = yes", "ok = yes"),
    Support = c(0.1, 0.2),
    Confidence = c(0.6, 0.7),
    Fitness = c(1.2, 1.3),
    stringsAsFactors = FALSE
  )
  parsed <- parse_rules(df)
  items <- parsed$items

  row1 <- items[items$label == "score >= 1e3", ]
  expect_equal(row1$feature, "score")
  expect_equal(row1$op, ">=")

  row2 <- items[items$label == "val in (1e-3, 2.5e-1]", ]
  expect_equal(row2$feature, "val")
  expect_true(is.finite(row2$lo) && is.finite(row2$hi))
})

test_that("missing required columns triggers an error", {
  df_bad <- data.frame(
    Antecedent = "A = 1",
    Consequence = "B = 2",
    Support = 0.1,
    Confidence = 0.5
    # fitness column missing
  )
  expect_error(parse_rules(df_bad), "missing required columns", fixed = FALSE)
})