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

# unsupervised discretization (cluster)
disc <- arules::discretizeDF(abalone, default = list(method = "cluster", breaks = 7))
tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", itemLabels(tr), value = TRUE)

rules <- apriori(
  tr,
  parameter = list(supp = 0.001, conf = 0.35, minlen = 3, maxlen = 10),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)
stopifnot(length(rules) > 0L)
q <- quality(rules)

# pick most frequent RHS then stratify by first/second LHS item
rhs_str <- labels(rhs(rules))
requested_rhs <- names(sort(table(rhs_str), decreasing = TRUE))[1]
r <- rules[rhs_str == requested_rhs]
q <- quality(r)

lhs_chr <- sub("^\\{(.*)\\}$", "\\1", labels(lhs(r)))
tok <- strsplit(lhs_chr, "\\s*,\\s*")
first  <- vapply(tok, `[`, "", 1L)
second <- vapply(tok, function(x) if (length(x)>=2) x[2] else NA_character_, "")

k_first  <- 12
k_second <- 8
k_leaf   <- 6

idx_all <- integer()
for (f in head(names(sort(table(first), decreasing = TRUE)), k_first)) {
  ix_f <- which(first == f)
  ix_f <- ix_f[order(q$lift[ix_f], q$confidence[ix_f], decreasing = TRUE, na.last = NA)]
  sec_vals <- head(names(sort(table(second[ix_f]), decreasing = TRUE)), k_second)
  for (s in sec_vals) {
    j <- ix_f[which(second[ix_f] == s)]
    if (length(j)) {
      j <- j[order(q$lift[j], q$confidence[j], decreasing = TRUE, na.last = NA)]
      idx_all <- c(idx_all, head(j, k_leaf))
    }
  }
}
idx_all <- unique(idx_all)
r_balanced <- r[idx_all]
stopifnot(length(r_balanced) > 0L)

# parse (no CSV), strip braces
df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(r_balanced))),
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(r_balanced))),
  Support     = as.numeric(quality(r_balanced)$support),
  Confidence  = as.numeric(quality(r_balanced)$confidence),
  Fitness        = as.numeric(quality(r_balanced)$lift),
  check.names = FALSE
)
parsed <- niarules::parse_rules(df)
layout <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")

# local domains (were commented out before)
dom <- niarules::metric_domains(layout$edges)

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
rgl::rgl.snapshot("test7.png", fmt = "png", top = TRUE