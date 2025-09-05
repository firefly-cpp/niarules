library(readr)
library(arules)
library(arulesCBA)
library(niarules)

set.seed(1248)

# --- data -----------------------------------------------------------------
path <- system.file("extdata", "Abalone.csv", package = "niarules")
abalone <- readr::read_csv(path, show_col_types = FALSE)
names(abalone) <- c("Sex","Length","Diameter","Height",
                    "WholeWeight","ShuckedWeight","VisceraWeight",
                    "ShellWeight","Rings")
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# supervised discretization w.r.t. Rings
disc <- discretizeDF.supervised(Rings ~ ., abalone)

# transactions restricted to predicting Rings=*
trans <- as(disc, "transactions")
rhs_labels <- grep("^Rings=", itemLabels(trans), value = TRUE)

# --- mine -----------------------------------------------------------------
rules <- apriori(
  trans,
  parameter = list(supp = 5e-4, conf = 0.4, minlen = 2, maxlen = 9),
  appearance = list(rhs = rhs_labels, default = "lhs"),
  control = list(verbose = FALSE)
)
stopifnot(length(rules) > 0L)

# choose a RHS to showcase (fallback to most frequent)
requested_rhs <- "Rings=23"
rhs_str <- labels(rhs(rules))
if (!requested_rhs %in% rhs_str) {
  rhs_counts <- sort(table(rhs_str), decreasing = TRUE)
  requested_rhs <- names(rhs_counts)[1]
  message("Requested RHS not found; using most frequent RHS: ", requested_rhs)
}

# balance within LHS-length buckets (top-k by lift/conf inside each length)
r <- rules[rhs_str == requested_rhs]
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

# --- build parse df (no CSV) ----------------------------------------------
df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(r_balanced))),  # strip {}
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(r_balanced))),
  Support     = as.numeric(quality(r_balanced)$support),
  Confidence  = as.numeric(quality(r_balanced)$confidence),
  Fitness     = as.numeric(quality(r_balanced)$lift),                  # use Lift name
  check.names = FALSE
)
parsed <- niarules::parse_rules(df)

# pick the “branchiest” RHS among those present (same scoring as before)
rhs_lab <- niarules::rhs_label_per_rule(parsed, ignore_order = TRUE)
df_rules <- data.frame(rhs_label = rhs_lab,
                       len = parsed$rules$antecedent_length,
                       stringsAsFactors = FALSE)
scores_list <- by(df_rules$len, df_rules$rhs_label, function(v) {
  c(n = length(v), mean_len = mean(v), share_len3 = mean(v >= 3))
})
scores <- as.data.frame(do.call(rbind, scores_list))
scores$rhs_label <- rownames(scores)
pick <- scores$rhs_label[order(scores$share_len3, scores$n, decreasing = TRUE)][1]

parsed_one <- niarules::filter_parsed_by_rhs(parsed, rhs_label = pick, ignore_order = TRUE)
layout     <- niarules::build_coral_plots(parsed_one, lhs_sort_metric = "confidence")

# local domains straight from edges via helper
dom <- niarules::metric_domains(layout$edges)

# --- render ----------------------------------------------------------------
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
rgl::rgl.snapshot("test4.png", fmt = "png", top = TRUE)