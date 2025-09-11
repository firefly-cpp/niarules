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

parsed = niarules::parse_rules(de$arules)

layout <- niarules::build_coral_plots(parsed)

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  grid_color = "grey80",
  legend     = FALSE,
  label_mode   = "none",
  edge_width_metric  = "support",
  edge_width_range = c(1, 5),
  edge_width_transform = "linear",
  edge_color_metric  = "support",
  edge_gradient = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
  edge_color_transform = "log",
  node_color_by        = "type",
  node_gradient        = c("#204060","#5B8BB5","#D7E6F2","#F5D0C6","#E57373","#991C1C")
)
#rgl::rgl.snapshot("test2.png", fmt = "png", top = TRUE)