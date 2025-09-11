library(testthat)

test_that("experimental renderer runs without error (smoke)", {
  withr::local_options(rgl.useNULL = TRUE)
  skip_on_cran(); skip_if_not_installed("rgl")
  p <- parse_rules(data.frame(
    Antecedent = c("A = a", "B = b"), Consequence = c("Y = y", "Y = y"),
    Support = c(0.2, 0.3), Confidence = c(0.9, 0.4), Fitness = c(2.2, 1.1)
  ))
  lay <- build_coral_plots(p)
  if (!rgl::rgl.cur()) rgl::open3d(useNULL = TRUE)
  expect_error(
    render_coral_rgl_experimental(lay$nodes, lay$edges, lay$grid_size, keep_camera = TRUE),
    NA
  )
})