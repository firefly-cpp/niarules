library(readr)
library(arules)
library(arulesCBA)

set.seed(1248)

abalone <- read_csv(
  "abalone_data.csv",
  col_names = c("Sex","Length","Diameter","Height",
                "WholeWeight","ShuckedWeight","VisceraWeight",
                "ShellWeight","Rings"),
  show_col_types = FALSE
)
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# supervised discretization (w.r.t. Rings)
disc <- discretizeDF.supervised(Rings ~ ., abalone)

# transactions restricted to predicting Rings=*
trans <- as(disc, "transactions")
rhs_labels <- grep("^Rings=", itemLabels(trans), value = TRUE)

# mine rules
rules <- apriori(
  trans,
  parameter = list(
    supp   = 5e-4,
    conf   = 0.4,
    minlen = 2,
    maxlen = 9
  ),
  appearance = list(rhs = rhs_labels, default = "lhs"),
  control    = list(verbose = FALSE)
)

if (length(rules) == 0L) stop("No rules mined with current thresholds.")

# choose a RHS to showcase
requested_rhs <- "Rings=23"  # try any; will fall back if absent

rhs_str <- labels(rhs(rules))
if (!requested_rhs %in% rhs_str) {
  # Pick the RHS with the most rules (richest coral)
  rhs_counts <- sort(table(rhs_str), decreasing = TRUE)
  requested_rhs <- names(rhs_counts)[1]
  message("Requested RHS not found; using most frequent RHS: ", requested_rhs)
}

r <- rules[rhs_str == requested_rhs]
if (length(r) == 0L) stop("Selected RHS produced zero rules.")

# rank within LHS length buckets and keep top-k per length
q <- quality(r)
lhs_len <- size(lhs(r))
split_len <- split(seq_along(r), lhs_len)

k_per_len <- 50          # tune: more/less density
keep_idx <- integer()
for (L in sort(unique(lhs_len))) {
  ids <- split_len[[as.character(L)]]
  if (length(ids)) {
    ord <- order(q$lift[ids], q$confidence[ids], decreasing = TRUE, na.last = NA)
    keep_idx <- c(keep_idx, ids[head(ord, k_per_len)])
  }
}
r_balanced <- unique(r[keep_idx])

if (length(r_balanced) == 0L) stop("After balancing, no rules left. Increase k_per_len or relax thresholds.")

write_rules_csv <- function(rules, file) {
  stopifnot(inherits(rules, "rules"))
  lhs_chr <- arules::labels(arules::lhs(rules))
  rhs_chr <- arules::labels(arules::rhs(rules))
  q       <- arules::quality(rules)
  
  # ensure required measures exist
  need <- c("support","confidence","lift")
  miss <- setdiff(need, names(q))
  if (length(miss)) {
    stop("rules quality() missing: ", paste(miss, collapse = ", "),
         "\nConsider computing missing measures with arules::interestMeasure().")
  }
  
  stopifnot(length(lhs_chr) == length(rhs_chr),
            nrow(q) == length(lhs_chr))
  
  out <- data.frame(
    Antecedent  = lhs_chr,
    Consequence = rhs_chr,                # keep combined RHS; your parser will split
    Support     = as.numeric(q$support),
    Confidence  = as.numeric(q$confidence),
    Fitness     = as.numeric(q$lift),     # your parser treats this as "lift"
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(out, file = file, row.names = FALSE)
  invisible(out)
}

write_rules_csv(r_balanced, "inst/extdata/abalone_rules.csv")

rules_path <- system.file("extdata", "abalone_rules.csv", package = "niarules", mustWork = TRUE)
df <- utils::read.csv(rules_path, stringsAsFactors = FALSE, check.names = FALSE)

strip_braces_one <- function(x) {
  s <- trimws(as.character(x))
  n <- nchar(s)
  has <- n >= 2L & substring(s, 1L, 1L) == "{" & substring(s, n, n) == "}"
  s[has] <- substring(s[has], 2L, n[has] - 1L)
  s
}

df$Antecedent  <- strip_braces_one(df$Antecedent)
df$Consequence <- strip_braces_one(df$Consequence)

parsed <- niarules::parse_rules(df)
table(parsed$rules$antecedent_length)

layout  <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")
#layout

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
  #node_radius_scale = 0.97, node_alpha_by_depth = TRUE
)

rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
rgl::par3d(antialias = 8)

rgl::rgl.snapshot("test6.png", fmt = "png", top = TRUE)