library(readr)
library(arules)
library(arulesCBA)

set.seed(1248)

abalone <- read_csv(
  "inst/extdata/Abalone.csv",
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

# make one RHS label per rule (optionally ignore order)
rhs_label_per_rule <- function(parsed, ignore_order = TRUE) {
  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  lab_by_id <- setNames(parsed$items$label, parsed$items$item_id)
  
  vapply(seq_len(nrow(parsed$rules)), function(i) {
    ids <- unlist(parsed$rules$rhs_item_ids[[i]], use.names = FALSE)
    if (!length(ids)) return("")
    labs <- lab_by_id[as.character(ids)]
    if (ignore_order) labs <- sort(labs)
    paste(labs, collapse = ", ")
  }, character(1))
}

# build one composite RHS label per rule (optionally ignoring item order)
rhs_label_per_rule <- function(parsed, ignore_order = TRUE) {
  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  lab_by_id <- setNames(parsed$items$label, parsed$items$item_id)
  
  vapply(seq_len(nrow(parsed$rules)), function(i) {
    ids <- unlist(parsed$rules$rhs_item_ids[[i]], use.names = FALSE)
    if (!length(ids)) return("")
    labs <- lab_by_id[as.character(ids)]
    if (ignore_order) labs <- sort(labs)
    paste(labs, collapse = ", ")
  }, character(1))
}

# filter a parsed ruleset to a single RHS label; drops unused items and remaps item_id
filter_parsed_by_rhs <- function(parsed, rhs_label, ignore_order = TRUE) {
  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  items <- parsed$items
  rules <- parsed$rules
  
  # compute per-rule RHS label
  rule_lab <- rhs_label_per_rule(parsed, ignore_order = ignore_order)
  
  keep <- rule_lab == rhs_label
  rules2 <- rules[keep, , drop = FALSE]
  if (nrow(rules2) == 0L) {
    stop("No rules match RHS label: ", rhs_label, call. = FALSE)
  }
  
  # collect used item_ids
  used_ids <- sort(unique(c(
    unlist(rules2$lhs_item_ids, use.names = FALSE),
    unlist(rules2$rhs_item_ids, use.names = FALSE)
  )))
  
  # remap item_id to 0..(k-1)
  id_map <- setNames(seq_along(used_ids) - 1L, as.character(used_ids))
  
  items2 <- items[items$item_id %in% used_ids, , drop = FALSE]
  items2$item_id <- unname(id_map[as.character(items2$item_id)])
  rownames(items2) <- NULL
  
  remap_list_ids <- function(lst) {
    lapply(seq_along(lst), function(i) {
      v <- unlist(lst[[i]], use.names = FALSE)
      if (!length(v)) integer()
      else unname(as.integer(id_map[as.character(v)]))
    })
  }
  
  rules2$lhs_item_ids <- remap_list_ids(rules2$lhs_item_ids)
  rules2$rhs_item_ids <- remap_list_ids(rules2$rhs_item_ids)
  class(rules2$lhs_item_ids) <- "AsIs"
  class(rules2$rhs_item_ids) <- "AsIs"
  rownames(rules2) <- NULL
  
  list(items = items2, rules = rules2)
}

df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(r_balanced))),  # strip {}
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(r_balanced))),
  Support     = as.numeric(quality(r_balanced)$support),
  Confidence  = as.numeric(quality(r_balanced)$confidence),
  Fitness     = as.numeric(quality(r_balanced)$lift),
  stringsAsFactors = FALSE
)

parsed  <- niarules::parse_rules(df)
# choose a RHS with both rings and branches
rhs_label <- rhs_label_per_rule(parsed, ignore_order = TRUE)

# score each RHS by how branchy it looks (more long LHS)
df_rules <- data.frame(rhs_label = rhs_label,
                       len = parsed$rules$antecedent_length,
                       stringsAsFactors = FALSE)

scores_list <- by(df_rules$len, df_rules$rhs_label, function(v) {
  c(n = length(v),
    mean_len = mean(v),
    share_len3 = mean(v >= 3))
})
scores <- do.call(rbind, scores_list)
scores <- as.data.frame(scores)
scores$rhs_label <- rownames(scores)

# pick RHS with highest share of len>=3 (break ties by n)
ord <- order(scores$share_len3, scores$n, decreasing = TRUE)
pick <- scores$rhs_label[ord][1]

parsed_one <- filter_parsed_by_rhs(parsed, rhs_label = pick, ignore_order = TRUE)
layout     <- niarules::build_coral_plots(parsed_one, lhs_sort_metric = "confidence")


# local domains for punchy contrast
dom <- list(
  confidence = range(layout$edges$confidence, finite = TRUE),
  support    = range(layout$edges$support,    finite = TRUE),
  lift       = range(layout$edges$lift,       finite = TRUE)
)

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
rgl::rgl.snapshot("test4.png", fmt = "png", top = TRUE)