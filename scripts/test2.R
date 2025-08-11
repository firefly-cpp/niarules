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

# parse the output data
parsed = parse_rules(de$arules)

# use the parsed data to build the plotting data
layout <- build_coral_plots(parsed)

# render the data with rgl
render_coral_rgl(
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
  node_color_by = "item",
  node_colors   = c(lhs1="#9E3D3D", lhs2="#006D77", lhs3="#8A5FBF", lhs4="#6E8000"),
  palette_hcl_c = 80,
  palette_hcl_l = 50
)