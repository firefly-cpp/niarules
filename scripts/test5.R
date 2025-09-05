library(readr)
library(arules)
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

# discretize: equal-frequency bins
disc <- arules::discretizeDF(abalone, default = list(method = "frequency", breaks = 6))
tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", arules::itemLabels(tr), value = TRUE)

# mine (and de-redundant)
rules <- arules::apriori(
  tr,
  parameter = list(supp = 0.002, conf = 0.5, minlen = 2, maxlen = 8),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)
rules <- rules[!arules::is.redundant(rules)]
stopifnot(length(rules) > 0L)

# rank by lift/conf/supp and cap per-RHS to avoid domination
q <- arules::quality(rules)
ord <- do.call(order, list(-q$lift, -q$confidence, -q$support, na.last = NA))
rhs_lab <- arules::labels(arules::rhs(rules))
split_ord <- split(ord, rhs_lab[ord])
k <- 250
keep <- unlist(lapply(split_ord, function(ix) head(ix, k)), use.names = FALSE)
rules <- rules[keep]

# build parse df (no CSV), strip braces
df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(rules))),
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(rules))),
  Support     = as.numeric(q$support),
  Confidence  = as.numeric(q$confidence),
  Fitness        = as.numeric(q$lift),
  check.names = FALSE
)
parsed <- niarules::parse_rules(df)
layout <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")

# render (kept your choices)
niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  label_mode = "interval_short", max_labels = 20,
  theme = "flat",
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1, 6),
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6", "#f7f7f7", "#d7191c"),
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.2, 1),
  node_color_by        = "item",
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)

#rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
#rgl::par3d(antialias = 8)
rgl::rgl.snapshot("test5.png", fmt = "png", top = TRUE)