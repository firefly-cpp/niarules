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

plots <- build_coral_plots(de$arules)

#plots$nodes
#plots$edges
#plots$grid_size

render_coral_rgl(
  plots$nodes, plots$edges,
  plots$grid_size,
  grid_color   = "lightblue",
  legend       = FALSE
)

