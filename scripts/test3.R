library(niarules)

# One plot (single RHS) with complex shared structure
df <- data.frame(
  Antecedent = c(
    # depth 1 spokes
    "lhs1 [0.2, 0.5)",             # lhs1 -> RHS
    "lhs2 = A",                    # lhs2 -> RHS
    "lhs3 in {X, Y}",              # lhs3 -> RHS
    "lhs4 > 10",                   # lhs4 -> RHS
    
    # depth 2 branches sharing A/B/C hubs
    "lhs2 = A, lhs1 [0.2, 0.5)",   # lhs2 -> lhs1 -> RHS
    "lhs3 in {X, Y}, lhs1 [0.2, 0.5)",
    "lhs4 > 10, lhs2 = A",
    "lhs5 < 3, lhs2 = A",
    "lhs6 >= 7, lhs3 in {X, Y}",
    "lhs7 = B, lhs3 in {X, Y}",
    
    # depth 3 (fanouts off the depth-2 hubs)
    "lhs8 [0.4, 0.9), lhs4 > 10, lhs2 = A",
    "lhs9 = C,         lhs4 > 10, lhs2 = A",
    "lhs10 in {u,v},   lhs5 < 3,  lhs2 = A",
    "lhs11 >= 12,      lhs6 >= 7, lhs3 in {X, Y}",
    "lhs12 = D,        lhs7 = B,  lhs3 in {X, Y}",
    "lhs13 < 1,        lhs1 [0.2, 0.5), lhs3 in {X, Y}",
    
    # depth 4 (two deeper chains to densify)
    "lhs14 [2, 5],     lhs8 [0.4, 0.9), lhs4 > 10, lhs2 = A",
    "lhs15 = E,        lhs9 = C,         lhs4 > 10, lhs2 = A",
    "lhs16 > 20,       lhs10 in {u,v},   lhs5 < 3,  lhs2 = A",
    "lhs17 in {m,n},   lhs11 >= 12,      lhs6 >= 7, lhs3 in {X, Y}",
    "lhs18 = F,        lhs12 = D,        lhs7 = B,  lhs3 in {X, Y}",
    
    # depth 5 (one long tendril)
    "lhs19 <= 0,       lhs14 [2, 5],     lhs8 [0.4, 0.9), lhs4 > 10, lhs2 = A"
  ),
  Consequence = "Outcome = yes, Test=true, HaveFun = 0.1",   # single RHS → single plot
  Support    = c(
    # depth 1
    0.22, 0.20, 0.18, 0.17,
    # depth 2
    0.14, 0.13, 0.12, 0.11, 0.11, 0.10,
    # depth 3
    0.085, 0.080, 0.078, 0.075, 0.072, 0.070,
    # depth 4
    0.055, 0.050, 0.048, 0.046, 0.044,
    # depth 5
    0.030
  ),
  Confidence = c(
    # rough, rising a bit with depth
    0.58, 0.60, 0.59, 0.57,
    0.62, 0.63, 0.61, 0.60, 0.60, 0.59,
    0.66, 0.65, 0.64, 0.66, 0.65, 0.63,
    0.70, 0.69, 0.68, 0.69, 0.68,
    0.72
  ),
  Fitness    = c(  # treat as "lift" in your pipeline
    1.3, 1.4, 1.35, 1.25,
    1.6, 1.55, 1.5, 1.45, 1.45, 1.42,
    1.75, 1.7, 1.68, 1.72, 1.7, 1.62,
    1.95, 1.9, 1.88, 1.9, 1.86,
    2.1
  ),
  stringsAsFactors = FALSE
)

parsed = parse_rules(df)
parsed

layout = build_coral_plots(parsed)
layout

render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  grid_color = "grey80",
  legend     = FALSE,
  label_mode   = "none",
  #label_cex    = 0.7,
  #label_offset = 1.5,
  #max_labels   = 100,
  edge_width_metric    = "support",
  edge_width_transform = "linear",
  edge_color_metric    = "lift",
  edge_color_transform = "linear",
  edge_gradient        = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
  edge_alpha_metric    = "confidence",
  edge_alpha_range     = c(0.1, 0.2),
  edge_alpha_transform = "linear",
  node_color_by = "item",
  node_gradient   = c(lhs1="#9E3D3D", lhs2="#006D77", lhs3="#8A5FBF", lhs4="#6E8000"),
  y_scale = 0.15, jitter_sd = 0.015, jitter_mode = "deterministic"
)