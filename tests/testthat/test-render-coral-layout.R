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
  
  expect_error(
    render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
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
    edge_width_metric    = "support",
    edge_width_range     = c(1.5, 6),
    edge_width_transform = "linear",
    edge_color_metric    = "lift",
    edge_gradient        = c("#003f5c", "#ffa600"),
    node_color_by        = "none",
    return_data          = TRUE
  )
  
  edges <- res$edges
  expect_true(all(c("support","confidence","lift","width","color") %in% names(edges)))
  expect_gte(min(edges$width), 1.5)
  expect_lte(max(edges$width), 6.0)
  
  # monotonic mapping wrt support (ties allowed)
  ord_w <- order(edges$width, decreasing = FALSE)
  expect_true(all(diff(rank(edges$support, ties.method = "min")[ord_w]) >= 0))
})

test_that("edge width transform changes width distribution", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
  
  lin <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                          edge_width_metric    = "confidence",
                          edge_width_transform = "linear",
                          node_color_by        = "none",
                          return_data          = TRUE)$edges$width
  sqr <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                          edge_width_metric    = "confidence",
                          edge_width_transform = "sqrt",
                          node_color_by        = "none",
                          return_data          = TRUE)$edges$width
  expect_false(isTRUE(all.equal(lin, sqr)))
})

test_that("changing edge_gradient or color metric changes edge colors", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
  
  c1 <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                         edge_color_metric = "lift",
                         edge_gradient     = c("#2166AC","#B2182B"),
                         node_color_by     = "none",
                         return_data       = TRUE)$edges$color
  c2 <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                         edge_color_metric = "lift",
                         edge_gradient     = c("#2b8cbe","#e34a33"),
                         node_color_by     = "none",
                         return_data       = TRUE)$edges$color
  expect_false(isTRUE(all.equal(c1, c2)))
  
  # also: changing the color metric (with same gradient) changes colors
  c3 <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                         edge_color_metric = "support",
                         edge_gradient     = c("#2166AC","#B2182B"),
                         node_color_by     = "none",
                         return_data       = TRUE)$edges$color
  expect_false(isTRUE(all.equal(c1, c3)))
})

test_that("width and color metrics are decoupled", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
  
  # same width metric, different color metric => widths equal, colors differ
  a <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                        edge_width_metric = "support",
                        edge_color_metric = "lift",
                        return_data       = TRUE)$edges
  b <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                        edge_width_metric = "support",
                        edge_color_metric = "confidence",
                        return_data       = TRUE)$edges
  expect_true(isTRUE(all.equal(a$width, b$width)))
  expect_false(isTRUE(all.equal(a$color, b$color)))
  
  # same color metric, different width metric => colors equal, widths differ
  c <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                        edge_width_metric = "support",
                        edge_color_metric = "lift",
                        return_data       = TRUE)$edges
  d <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                        edge_width_metric = "confidence",
                        edge_color_metric = "lift",
                        return_data       = TRUE)$edges
  expect_true(isTRUE(all.equal(c$color, d$color)))
  expect_false(isTRUE(all.equal(c$width, d$width)))
})

#test_that("alpha mapping works when edge_alpha_metric is provided", {
#  skip_if_not_installed("rgl")
#  df <- mk_df()
#  parsed <- parse_rules(df)
#  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
#  
#  e <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
#                       edge_width_metric    = "support",
#                        edge_color_metric    = "lift",
#                        edge_alpha_metric    = "confidence",
#                        edge_alpha_range     = c(0.2, 0.9),
#                        edge_alpha_transform = "linear",
#                        node_color_by        = "none",
#                        return_data          = TRUE)$edges
#  # extract alpha channel (0..255) and check it varies
#  al <- grDevices::col2rgb(e$color, alpha = TRUE)[4, ]
#  expect_true(length(unique(al)) > 1)
#})

test_that("node coloring by type vs item and overrides", {
  skip_if_not_installed("rgl")
  df <- mk_df()
  parsed <- parse_rules(df)
  layout <- build_coral_plots(parsed, lhs_sort_metric = "confidence")
  
  # by type (base feature)
  n_type <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                             node_color_by = "type",
                             return_data   = TRUE)$nodes
  expect_true("color" %in% names(n_type))
  expect_gte(length(unique(n_type$color)), length(unique(n_type$feature)))
  
  # by item (full label)
  n_item <- render_coral_rgl(layout$nodes, layout$edges, layout$grid_size,
                             node_color_by = "item",
                             return_data   = TRUE)$nodes
  expect_gte(length(unique(n_item$color)), length(unique(n_item$item)))
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
                          return_data   = TRUE)
  expect_true(all(res$nodes$color == "#123456"))
})