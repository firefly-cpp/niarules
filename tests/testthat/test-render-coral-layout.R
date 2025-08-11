library(testthat)

# use headless rgl for CI
setup({
  skip_if_not_installed("rgl")
  old <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old), add = TRUE)
  
})

mk_df <- function() {
  data.frame(
    Antecedent  = c('A = a','B = b','A = a, B = b','C in [1, 2]','D >= 10'),
    Consequence = c('Y = y1','Y = y1','Y = y1','{Z = z1, W = w1}','Z = z2'),
    # chosen so confidence favors A>B, support favors B>A
    Support     = c(0.20, 0.60, 0.30, 0.20, 0.10),
    Confidence  = c(0.90, 0.40, 0.80, 0.60, 0.30),
    Fitness     = c(2.20, 1.10, 2.00, 1.50, 0.90),  # treated as lift
    stringsAsFactors = FALSE
  )
}

test_that("render_coral_rgl runs headless and draws shapes", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

  # smoke test: should not error and should create some shapes
  expect_error(
    render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                     edge_metric = "confidence",
                     node_color_by = "type"),
    NA
  )
  ids <- rgl::rgl.ids()
  expect_true(NROW(ids) > 0)
})

test_that("edge widths map to chosen metric and range (via return_data)", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

  res <- render_coral_rgl(
    layout$nodes, layout$edges, layout$grid_size,
    edge_metric = "support",
    edge_width_range = c(1.5, 6),
    edge_width_transform = "linear",
    edge_gradient = c("#003f5c", "#ffa600"),
    node_color_by = "none",
    return_data = TRUE
  )

  edges <- res$edges
  expect_true(all(c("support","confidence","lift","width","color") %in% names(edges)))
  expect_gte(min(edges$width), 1.5)
  expect_lte(max(edges$width), 6.0)

  # monotonic mapping wrt support (ties allowed)
  ord_m <- order(edges$support, decreasing = FALSE)
  ord_w <- order(edges$width,   decreasing = FALSE)
  expect_true(all(diff(rank(edges$support, ties.method = "min")[ord_w]) >= 0))
})

test_that("edge transforms change width distribution", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

  lin <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                          edge_metric = "confidence",
                          edge_width_transform = "linear",
                          node_color_by = "none",
                          return_data = TRUE)$edges$width
  sqr <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                          edge_metric = "confidence",
                          edge_width_transform = "sqrt",
                          node_color_by = "none",
                          return_data = TRUE)$edges$width
  expect_false(isTRUE(all.equal(lin, sqr)))
})

test_that("changing edge_gradient changes edge colors", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

  c1 <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                         edge_metric = "lift",
                         edge_gradient = c("#2166AC","#B2182B"),
                         node_color_by = "none",
                         return_data = TRUE)$edges$color
  c2 <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                         edge_metric = "lift",
                         edge_gradient = c("#2b8cbe","#e34a33"),
                         node_color_by = "none",
                         return_data = TRUE)$edges$color
  expect_false(isTRUE(all.equal(c1, c2)))
})

test_that("node coloring by type vs item and overrides", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

  # by type (base feature)
  n_type <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                             node_color_by = "type",
                             return_data = TRUE)$nodes
  expect_true("color" %in% names(n_type))
  expect_gte(length(unique(n_type$color)), length(unique(n_type$feature)))

  # by item (full label)
  n_item <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                             node_color_by = "item",
                             return_data = TRUE)$nodes
  expect_gte(length(unique(n_item$color)), length(unique(n_item$item)))

  # override one label color
  some_item <- n_item$item[1]
  over <- setNames("#FF0000", some_item)
  n_over <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                             node_color_by = "item",
                             node_colors   = over,
                             return_data   = TRUE)$nodes
  # should be TRUE now:
  all(n_over$color[n_over$item == some_item] == "#FF0000")
})

test_that("node_color_by = 'none' doesn't overwrite existing node colors", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
  nodes0 <- layout$nodes
  nodes0$color <- "#123456"

  res <- render_coral_rgl(nodes0, layout$edges, layout$grid_size,
                          node_color_by = "none",
                          return_data = TRUE)
  expect_true(all(res$nodes$color == "#123456"))
})

