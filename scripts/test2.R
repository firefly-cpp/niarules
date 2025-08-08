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

#1
plots <- build_coral_plots(de$arules)
render_coral_rgl(plots$nodes, plots$edges, plots$grid_size, label_mode="interval_short", legend=TRUE, max_labels=0)

#2
edge_pal <- c("#440154","#3B528B","#21908C","#5DC863","#FDE725")
type_colors <- c(Sex="#009378", Length="#6E8000")

plots <- build_coral_plots(
  de$arules,
  edge_metric   = "lift",
  edge_gradient = edge_pal,
  node_color_by = "type",
  node_colors   = type_colors,   # partial mapping OK
  palette_hcl_c = 80,
  palette_hcl_l = 50
)

render_coral_rgl(plots$nodes, plots$edges, plots$grid_size, label_mode="interval_short", legend=FALSE, max_labels=0)

#3
plots <- build_coral_plots(
  de$arules,
  node_color_by = "item"
)
render_coral_rgl(plots$nodes, plots$edges, plots$grid_size, label_mode="item", legend=TRUE, max_labels=0)

#4
plots <- build_coral_plots(
  de$arules,
  edge_metric   = "support",
  edge_gradient = c("#2c7bb6","#d7191c"),
  node_color_by = "none"
)
render_coral_rgl(plots$nodes, plots$edges, plots$grid_size, label_mode="interval_short", legend=TRUE)