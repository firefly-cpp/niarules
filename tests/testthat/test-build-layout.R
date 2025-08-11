# tests/testthat/test-build-layout.R
library(testthat)

# minimal helper to make a layout-focused rules df
# we deliberately set single-antecedent metrics so that changing lhs_sort_metric
# will change the order of A vs B in the 2-item rule.
mk_layout_df <- function() {
  data.frame(
    Antecedent = c(
      'A = a',                    # single antecedent A
      'B = b',                    # single antecedent B
      'A = a, B = b',             # two antecedents
      'C in [1, 2]',              # numeric interval
      'D >= 10'                   # relational
    ),
    Consequence = c(
      'Y = y1',                   # same RHS as rules 1-3 (one combined plot)
      'Y = y1',
      'Y = y1',
      '{Z = z1, W = w1}',         # multi-RHS to exercise stacked roots
      'Z = z2'                    # different combined RHS (third plot)
    ),
    # Metrics tuned so:
    # - Confidence: A (0.9) > B (0.4) ⇒ order A, B
    # - Support:    B (0.6) > A (0.2) ⇒ order B, A
    Support    = c(0.20, 0.60, 0.30, 0.20, 0.10),
    Confidence = c(0.90, 0.40, 0.80, 0.60, 0.30),
    Fitness    = c(2.20, 1.10, 2.00, 1.50, 0.90),  # treated as lift
    stringsAsFactors = FALSE
  )
}

test_that("build_layout returns nodes/edges with expected columns and grid_size", {
  df <- mk_layout_df()
  parsed <- parse_rules(df)
  layout <- build_layout(parsed, lhs_sort_metric = "confidence", edge_metric = "confidence")

  expect_type(layout, "list")
  expect_true(all(c("nodes","edges","edge_metric","edge_range","grid_size") %in% names(layout)))

  nodes <- layout$nodes
  edges <- layout$edges

  expect_s3_class(nodes, "data.frame")
  expect_s3_class(edges, "data.frame")

  expect_true(nrow(nodes) > 0)
  expect_true(nrow(edges) > 0)

  # nodes columns (geometry + metadata exposed by C++)
  expect_true(all(c(
    "x","y","z","radius","id","item","step","feature","kind",
    "interval_low","interval_high","incl_low","incl_high",
    "category_val","interval_label","interval_label_short",
    "x_offset","z_offset"
  ) %in% names(nodes)))

  # edges columns (geometry + styling)
  expect_true(all(c("x","y","z","x_end","y_end","z_end","width","color") %in% names(edges)))
  expect_true(is.numeric(edges$width))
  expect_true(all(nchar(as.character(edges$color)) %in% c(7L, 9L))) # #RRGGBB or #AARRGGBB

  # grid_size should be ceil(sqrt(#unique combined RHS))
  # Here: "Y = y1", "Z = z1, W = w1", "Z = z2" => 3 unique -> grid_size = 2
  expect_equal(layout$grid_size, 2L)

  # edge metadata pass-through
  expect_equal(layout$edge_metric, "confidence")
  expect_true(is.numeric(layout$edge_range))
  expect_equal(length(layout$edge_range), 2L)
  expect_true(all(is.finite(layout$edge_range)))
})

test_that("multi-RHS produces multiple root nodes at the same plot center", {
  df <- mk_layout_df()
  parsed <- parse_rules(df)
  layout <- build_layout(parsed, lhs_sort_metric = "confidence")

  nodes <- layout$nodes
  expect_true("step" %in% names(nodes))
  roots <- nodes[nodes$step == 0L, ]

  # Group roots by plot center (x_offset, z_offset)
  key <- paste0(round(roots$x_offset, 6), "_", round(roots$z_offset, 6))
  tab <- table(key)
  # Expect at least one center with >1 root (from "{Z = z1, W = w1}")
  expect_true(any(tab > 1L))
})

test_that("changing lhs_sort_metric changes geometry (A vs B order flips)", {
  df <- mk_layout_df()
  parsed <- parse_rules(df)

  layout_conf <- build_layout(parsed, lhs_sort_metric = "confidence")
  layout_supp <- build_layout(parsed, lhs_sort_metric = "support")

  # we don't rely on internal IDs; just compare edge geometry wholesale.
  e1 <- layout_conf$edges[, c("x","y","z","x_end","y_end","z_end")]
  e2 <- layout_supp$edges[, c("x","y","z","x_end","y_end","z_end")]

  # round to reduce floating-point noise
  r1 <- as.matrix(round(e1, 6))
  r2 <- as.matrix(round(e2, 6))

  # expect not all equal (different LHS ordering ⇒ different bundling/coords)
  expect_false(isTRUE(all.equal(r1, r2)))
})

test_that("edge_metric selection propagates to output metadata", {
  df <- mk_layout_df()
  parsed <- parse_rules(df)

  layout_lift <- build_layout(parsed, lhs_sort_metric = "confidence", edge_metric = "lift")
  expect_equal(layout_lift$edge_metric, "lift")
  expect_equal(length(layout_lift$edge_range), 2L)
  expect_true(is.numeric(layout_lift$edge_range))
})
