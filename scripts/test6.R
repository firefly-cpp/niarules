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
  parameter = list(supp = 0.002, conf = 0.4, minlen = 2, maxlen = 5),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)
rules <- rules[!arules::is.redundant(rules)]
stopifnot(length(rules) > 0L)

rhs_txt <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules)))
rules <- rules#[rhs_txt == "Rings=9"]  # select the ring we're interested in
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
layout <- niarules::build_coral_plots(parsed)

feat_levels <- sort(unique(na.omit(layout$nodes$feature)))
pal_nodes <- grDevices::hcl.colors(length(feat_levels), "Purple-Yellow")
names(pal_nodes) <- feat_levels

pal_edges <- c("#2166ac", "#f0f0f0", "#b2182b")

out <- niarules::render_coral_rgl(
  nodes = layout$nodes, edges = layout$edges, grid_size = layout$grid_size,
  legend = TRUE, y_scale = 0.2,
  node_color_by = "type", node_gradient = pal_nodes,
  node_gradient_map = "even",
  edge_width_metric = "support", edge_width_transform = "sqrt",
  edge_width_range = c(2, 8),
  edge_color_metric = "lift", edge_color_transform = "log",
  edge_gradient = pal_edges,
  edge_alpha_metric = "confidence", edge_alpha_transform = "linear",
  edge_alpha_range = c(0.35, 1) 
)
#rgl::rgl.snapshot("test6.png", top=TRUE)