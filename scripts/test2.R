library(niarules)

data_raw <- niarules::read_dataset("inst/extdata/Abalone.csv")
features <- niarules::extract_feature_info(data_raw)
d <- niarules::problem_dimension(features, is_time_series = FALSE)

de <- niarules::differential_evolution(
  d        = d,
  np       = 30,
  f        = 0.5,
  cr       = 0.9,
  nfes     = 1000,
  features = features,
  data     = data_raw,
  is_time_series = FALSE
)

plots <- build_radial_plots(de$arules)
render_radial_rgl(
  plots$nodes, plots$edges,
  plots$grid_size,
  feature_cols = NULL,
  grid_color   = "lightblue",
  legend       = FALSE
)

