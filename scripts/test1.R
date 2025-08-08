library(niarules)

# Shared-prefix toy rules
shared_rules <- data.frame(
  Antecedent = c(
    "A",                  # A -> RHS
    "B, A",               # B -> A -> RHS
    "C, A",               # C -> A -> RHS
    "D, C, A",             # D -> C -> A -> RHS
    "E",             # E -> RHS
    "F"             # F -> RHS
  ),
  Consequence = c(
    "{Target = yes}",
    "{Target = yes}",
    "{Target = yes}",
    "{Target = yes}",
    "{Target = yes}",
    "{Target = yes}"
  ),
  Support    = c(0.20, 0.12, 0.10, 0.06, 0.10, 0.20),
  Confidence = c(0.60, 0.65, 0.62, 0.70, 0.2, 0.3),
  Fitness    = c(1.5, 1.8, 1.7, 2.1, 1.2, 1.8),   # treated like "lift"
  stringsAsFactors = FALSE
)

edge_pal    <- c("#440154","#3B528B","#21908C","#5DC863","#FDE725")
type_colors <- c(A="#6E8000", B="#009378", C="#3366CC", D="#AA33FF")  # optional overrides

plots <- build_coral_plots(
  arules        = shared_rules,
  edge_metric   = "lift",
  edge_gradient = edge_pal,
  node_color_by = "type",    # colors by parsed feature (here: same as item)
  node_colors   = type_colors
)
render_coral_rgl(plots$nodes, plots$edges, plots$grid_size, label_mode="item", legend=TRUE, max_labels=0)