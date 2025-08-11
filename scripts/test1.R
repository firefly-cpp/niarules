library(niarules)

df <- data.frame(
  Antecedent = c(
    "A",                  # A -> RHS
    "B, A",               # B -> A -> RHS
    "C, A",               # C -> A -> RHS
    "D, C, A",             # D -> C -> A -> RHS
    "E",             # E -> RHS
    "F"             # F -> RHS
  ),
  Consequence = c(
    "Target = yes",
    "Target = yes",
    "Target = yes",
    "Target = yes",
    "Target = yes",
    "Target = yes"
  ),
  Support    = c(0.20, 0.12, 0.10, 0.06, 0.10, 0.20),
  Confidence = c(0.60, 0.65, 0.62, 0.70, 0.2, 0.3),
  Fitness    = c(1.5, 1.8, 1.7, 2.1, 1.2, 1.8),   # treated like "lift"
  stringsAsFactors = FALSE
)

parsed = parse_rules(df)
#parsed

layout = build_coral_plots(parsed)
#layout

render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  grid_color = "grey80",
  legend     = TRUE,
  label_mode   = "item",
  label_cex    = 0.7,
  label_offset = 1.5,
  max_labels   = 100,
  edge_width_metric  = "lift",
  edge_width_range = c(1, 5),
  edge_width_transform = "linear",
  edge_gradient = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
  edge_alpha   = 0.6,
  node_color_by = "type",
  node_gradient   = c(lhs1="#9E3D3D", lhs2="#006D77", lhs3="#8A5FBF", lhs4="#6E8000"),
)