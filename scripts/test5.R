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
  lhs_sort_metric = "confidence"
  #bin_breaks = bin_breaks,
  #bin_digits = 3
)

L <- layout$edges$lift
L <- L[is.finite(L) & L > 0]
m <- max(abs(log(L)))                  # use the extremes, not quantiles
if (!is.finite(m) || m == 0) m <- 0.10 # ensure some spread
dom_lift_sym <- exp(c(-m, m))          # symmetric around lift=1

#dom <- niarules::metric_domains(layout$edges)

feat_levels <- sort(unique(na.omit(layout$nodes$feature)))

pal_plasma <- grDevices::hcl.colors(length(feat_levels), "Purple-Yellow")
names(pal_plasma) <- feat_levels

edge_pal <- c("#2166ac", "#f0f0f0", "#b2182b")

out <- niarules::render_coral_rgl(
  
  layout$nodes, layout$edges, layout$grid_size,

  legend = TRUE,
  
  label_mode = "item",
  label_offset = -0.5,
  max_labels = nrow(layout$nodes),
  label_cex = 0.6,

  node_color_by = "type",
  node_gradient = pal_plasma,
  node_gradient_map = "even",

  edge_width_metric = "support",
  edge_width_transform = "sqrt",
  edge_width_range = c(2, 8),

  edge_color_metric = "lift",
  edge_color_transform = "log",
  edge_gradient = edge_pal,

  edge_alpha_metric = "confidence",
  edge_alpha_transform = "linear",
  edge_alpha_range = c(0.35, 1),

  y_scale = 0.2,
  jitter_sd = 0.02,
  jitter_mode = "random",
  jitter_seed = 1248,
  
  return_data = TRUE
)

#rgl::rgl.snapshot("test5.png", top=TRUE)