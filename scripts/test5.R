library(readr)
library(arules)
library(niarules)

set.seed(1248)

levels_to_breaks <- function(levels_chr) {
  pr <- t(vapply(levels_chr, function(s) {
    m <- regmatches(s, gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?\\d+)?", s))[[1]]
    as.numeric(m[1:2])
  }, numeric(2)))
  sort(unique(c(pr[,1], pr[,2])))
}

path <- system.file("extdata", "Abalone.csv", package = "niarules")
abalone <- readr::read_csv(path, show_col_types = FALSE)
names(abalone) <- c("Sex","Length","Diameter","Height",
                    "WholeWeight","ShuckedWeight","VisceraWeight",
                    "ShellWeight","Rings")
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

disc <- arules::discretizeDF(abalone, default = list(method = "frequency", breaks = 6))
tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", arules::itemLabels(tr), value = TRUE)

num_feats <- c("Length","Diameter","Height","WholeWeight",
               "ShuckedWeight","VisceraWeight","ShellWeight")
bin_legend_apriori <- do.call(rbind, lapply(num_feats, function(f) {
  lv <- levels(disc[[f]])
  data.frame(feature = f,
             bin = seq_along(lv),
             interval = lv,
             stringsAsFactors = FALSE)
}))

rules_all <- arules::apriori(
  tr,
  parameter = list(supp = 0.0025, conf = 0.45, minlen = 2, maxlen = 8),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)
stopifnot(length(rules_all) > 0L)

rhs_txt <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules_all)))
rules <- rules_all[rhs_txt == "Rings=9"]  # select the ring we're interested in
rules

q  <- arules::quality(rules)
lhs <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::lhs(rules)))
rhs <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules)))
df <- data.frame(
  Antecedent  = lhs,
  Consequence = rhs,
  Support     = as.numeric(q$support),
  Confidence  = as.numeric(q$confidence),
  Fitness     = as.numeric(q$lift),
  check.names = FALSE
)

topK <- df |>
  dplyr::arrange(dplyr::desc(Fitness), dplyr::desc(Confidence), dplyr::desc(Support)) |>
  dplyr::slice_head(n = 12)

parsed <- niarules::parse_rules(df)
parsed

num_feats <- c("Length","Diameter","Height","WholeWeight",
               "ShuckedWeight","VisceraWeight","ShellWeight")

bin_breaks <- setNames(
  lapply(num_feats, function(f) levels_to_breaks(levels(disc[[f]]))),
  num_feats
)

layout <- niarules::build_coral_plots(
  parsed,
  lhs_sort_metric = "confidence",
  bin_breaks = bin_breaks,
  bin_digits = 3
)

L <- layout$edges$lift
L <- L[is.finite(L) & L > 0]
m <- max(abs(log(L)))                  # use the extremes, not quantiles
if (!is.finite(m) || m == 0) m <- 0.10 # ensure some spread
dom_lift_sym <- exp(c(-m, m))          # symmetric around lift=1

dom <- niarules::metric_domains(layout$edges)

feat_levels <- sort(unique(na.omit(layout$nodes$feature)))

pal_plasma <- grDevices::hcl.colors(length(feat_levels), "Purple-Yellow")
names(pal_plasma) <- feat_levels

edge_pal <- c("#2166ac", "#f0f0f0", "#b2182b")

out <- niarules::render_coral_rgl(
  
  layout$nodes, layout$edges, layout$grid_size,
  
  grid_outline = FALSE,
  
  theme = "default",
  theme_overrides = NULL,
  apply_theme = TRUE,
  
  legend = TRUE,
  legend_style = "feature_bins",
  legend_panel_width = 0.3,
  legend_panel_margin = 0.0001,
  legend_reserve = 0.3,
  legend_title_cex = 0.9, 
  legend_row_cex = 0.75,
  legend_pos = "custom",
  legend_xy = c(1.025, 1.0),
  legend_col_gap = 0.02,
  
  legend_cex = 1.0,
  legend_items_per_feature = 6L,
  legend_features_max = 10L,
  
  bin_label_fmt = "index",
  bin_legend = layout$bin_legend,
  bin_breaks = NULL,
  bin_infer = TRUE,
  
  label_mode = "bin",
  label_non_numeric = "category",
  label_offset = -0.5,
  label_color = "black",
  max_labels = nrow(layout$nodes),
  label_cex = 0.6,

  node_color_by = "type",
  node_gradient = pal_plasma,
  node_gradient_map = "even",
  node_scale = 1.5,
  
  edge_width_metric = "support",
  edge_width_transform = "sqrt",
  edge_width_range = c(2, 8),
  edge_width_domain = dom$support,
  
  edge_color_metric = "lift",
  edge_color_transform = "log",
  edge_gradient = edge_pal,
  edge_color_domain = dom_lift_sym,
  
  edge_alpha_metric = "confidence",
  edge_alpha_transform = "linear",
  edge_alpha_range = c(0.35, 1),
  edge_alpha_domain = dom$confidence,
  
  radial_gamma = 0.9,
  radial_expand = 1.5,
  
  y_scale = 0.2,
  jitter_sd = 0.02,
  jitter_mode = "random",
  jitter_seed = 1248,
  
  view_zoom = 0.65,
  view_theta = 0,
  view_phi = 45,
  
  return_data = TRUE
)

rgl::rgl.snapshot("test5.png", top=TRUE)