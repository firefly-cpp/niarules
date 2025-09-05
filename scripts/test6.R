library(readr)
library(arules)
library(arulesCBA)
library(niarules)

set.seed(1248)

# data
path <- system.file("extdata", "Abalone.csv", package = "niarules")
abalone <- readr::read_csv(path, show_col_types = FALSE)
names(abalone) <- c("Sex","Length","Diameter","Height",
                    "WholeWeight","ShuckedWeight","VisceraWeight",
                    "ShellWeight","Rings")
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# supervised discretization
disc <- discretizeDF.supervised(Rings ~ ., abalone)
trans <- as(disc, "transactions")
rhs_labels <- grep("^Rings=", itemLabels(trans), value = TRUE)

# mine
rules <- apriori(
  trans,
  parameter = list(supp = 5e-4, conf = 0.4, minlen = 2, maxlen = 9),
  appearance = list(rhs = rhs_labels, default = "lhs"),
  control = list(verbose = FALSE)
)
stopifnot(length(rules) > 0L)

# pick RHS (requested or most frequent)
requested_rhs <- "Rings=23"
rhs_str <- labels(rhs(rules))
if (!requested_rhs %in% rhs_str) {
  rhs_counts <- sort(table(rhs_str), decreasing = TRUE)
  requested_rhs <- names(rhs_counts)[1]
  message("Requested RHS not found; using most frequent RHS: ", requested_rhs)
}
r <- rules[rhs_str == requested_rhs]
stopifnot(length(r) > 0L)

# balance within LHS length
q <- quality(r)
lhs_len <- size(lhs(r))
split_len <- split(seq_along(r), lhs_len)
k_per_len <- 50
keep_idx <- integer()
for (L in sort(unique(lhs_len))) {
  ids <- split_len[[as.character(L)]]
  if (length(ids)) {
    ord <- order(q$lift[ids], q$confidence[ids], decreasing = TRUE, na.last = NA)
    keep_idx <- c(keep_idx, ids[head(ord, k_per_len)])
  }
}
r_balanced <- unique(r[keep_idx])
stopifnot(length(r_balanced) > 0L)

# parse (no CSV), strip braces
df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(r_balanced))),
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(r_balanced))),
  Support     = as.numeric(quality(r_balanced)$support),
  Confidence  = as.numeric(quality(r_balanced)$confidence),
  Fitness     = as.numeric(quality(r_balanced)$lift),
  check.names = FALSE
)
parsed <- niarules::parse_rules(df)

# build single-plot via helper
rhs_label <- niarules::rhs_label_per_rule(parsed, TRUE)[1]  # any present label
parsed_one <- niarules::filter_parsed_by_rhs(parsed, rhs_label, ignore_order = TRUE)
layout     <- niarules::build_coral_plots(parsed_one, lhs_sort_metric = "confidence")

# compute domains (these were missing before)
dom <- niarules::metric_domains(layout$edges)

# render (now with dom)
niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  label_mode   = "item", max_labels = 0, label_color = "grey20",
  
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1.2, 7.0),  edge_width_domain = dom$support,
  
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6","#f7f7f7","#d7191c"),
  edge_color_domain    = dom$lift,
  
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.15, 1),
  edge_alpha_transform = "linear",     edge_alpha_domain = dom$confidence,
  
  node_color_by        = "item",
  y_scale = 0.18, jitter_sd = 0.012, jitter_mode = "random", jitter_seed = 1248,
  radial_expand = 1.35, radial_gamma = 1.45
)

rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
rgl::par3d(antialias = 8)
rgl::rgl.snapshot("test6.png", fmt = "png", top = TRUE)
