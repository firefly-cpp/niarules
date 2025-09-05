library(niarules)
library(arules)     # for discretization helpers
library(dplyr)

set.seed(1248)

# --- 1) Data prep -----------------------------------------------------------
# Read csv shipped with niarules
raw <- niarules::read_dataset(system.file("extdata","Abalone.csv", package = "niarules"))

# Make sure Rings is a *categorical* target (integer levels).
# Abalone Rings are integers already; just coerce to factor so DE treats it as categorical.
raw$Rings <- factor(as.integer(raw$Rings))

# (Optional but helpful) discretize predictors *supervised* by Rings.
# This keeps Rings as-is and turns the other columns into interval factors,
# which tends to produce clearer, “branchy” antecedents.
data_de <- arules::discretizeDF.supervised(Rings ~ ., as.data.frame(raw))

# Build niarules structures
features <- niarules::extract_feature_info(data_de)
d        <- niarules::problem_dimension(features, is_time_series = FALSE)

# --- 2) A short DE miner with a small “rescue” bump -------------------------
mine_de_short <- function(seed, np = 18L, nfes = 360L, f = 0.6, cr = 0.9) {
  set.seed(seed)
  run_once <- function(np, nfes, f, cr) {
    de <- niarules::differential_evolution(
      d = d, np = np, f = f, cr = cr, nfes = nfes,
      features = features, data = data_de, is_time_series = FALSE
    )
    de
  }
  de <- tryCatch(run_once(np, nfes, f, cr), error = function(e) NULL)
  if (is.null(de) || is.null(de$arules)) {
    # gentle bump if the first try yields nothing
    de <- tryCatch(run_once(np + 6L, as.integer(nfes * 1.5), 0.7, 0.95), error = function(e) NULL)
  }
  de
}

# Harvest a handful of quick runs (sequential is fine to start)
seeds    <- 1:6
runs     <- lapply(seeds, mine_de_short)
have_any <- vapply(runs, function(x) !is.null(x) && !is.null(x$arules), logical(1))
stopifnot(any(have_any))
runs <- runs[have_any]

# Parse: parse_rules() accepts the native niarules rules object and does
# the temp-CSV bridge internally, so just give it de$arules per run.
parsed_list <- lapply(runs, function(x) niarules::parse_rules(x$arules))

# --- 3) Keep only rules with a *single* RHS item and that item is Rings -----
keep_rings_single_rhs <- function(parsed) {
  items <- parsed$items; rules <- parsed$rules
  # Get single RHS item-id or NA
  rhs_id <- vapply(rules$rhs_item_ids, function(v) if (length(v) == 1L) v[[1]] else NA_integer_, integer(1))
  is_single <- !is.na(rhs_id)
  # Map id -> feature, then test for "Rings"
  feat_by_id <- setNames(items$feature, items$item_id)
  is_rings   <- feat_by_id[as.character(rhs_id)] == "Rings"
  keep <- which(is_single & is_rings)
  
  if (!length(keep)) return(NULL)
  
  # Build a compact parsed list for just those rules (remap item_ids)
  niarules::filter_parsed_by_rhs(
    # We’ll pass labels one at a time later; here we just drop non-Rings rules first.
    # To reuse remapping logic, quickly stitch a single mega-RHS label per kept rule:
    # but simpler: subset rules directly and remap ourselves:
    parsed,
    rhs_label = "",  # dummy (we're not using this branch)
    ignore_order = TRUE
  )
}

# Instead of the helper above (which expects one RHS label),
# do the filtering explicitly and remap (copy/pasteable, no dependency on internals):
filter_to_rings_single_rhs <- function(parsed) {
  items <- parsed$items; rules <- parsed$rules
  rhs_id <- vapply(rules$rhs_item_ids, function(v) if (length(v) == 1L) v[[1]] else NA_integer_, integer(1))
  is_single <- !is.na(rhs_id)
  feat_by_id <- setNames(items$feature, items$item_id)
  is_rings   <- feat_by_id[as.character(rhs_id)] == "Rings"
  keep <- which(is_single & is_rings)
  if (!length(keep)) return(NULL)
  
  rules2 <- rules[keep, , drop = FALSE]
  used   <- sort(unique(unlist(c(rules2$lhs_item_ids, rules2$rhs_item_ids), use.names = FALSE)))
  id_map <- setNames(seq_along(used) - 1L, as.character(used))
  
  remap <- function(lst) lapply(lst, function(v) unname(as.integer(id_map[as.character(v)])))
  rules2$lhs_item_ids <- structure(remap(rules2$lhs_item_ids), class = "AsIs")
  rules2$rhs_item_ids <- structure(remap(rules2$rhs_item_ids), class = "AsIs")
  
  items2 <- items[items$item_id %in% used, , drop = FALSE]
  items2$item_id <- unname(as.integer(id_map[as.character(items2$item_id)]))
  rownames(items2) <- NULL; rownames(rules2) <- NULL
  
  list(items = items2, rules = rules2)
}

parsed_rings <- Filter(Negate(is.null), lapply(parsed_list, filter_to_rings_single_rhs))
stopifnot(length(parsed_rings) > 0)

# Merge all ring-only rulesets together (same schema)
merge_parsed <- function(lst) {
  it <- do.call(rbind, lapply(lst, `[[`, "items"))
  ru <- do.call(rbind, lapply(lst, `[[`, "rules"))
  # Recompact ids after the bind:
  used   <- sort(unique(unlist(c(ru$lhs_item_ids, ru$rhs_item_ids), use.names = FALSE)))
  id_map <- setNames(seq_along(used) - 1L, as.character(used))
  remap  <- function(lst) lapply(lst, function(v) unname(as.integer(id_map[as.character(v)])))
  ru$lhs_item_ids <- structure(remap(ru$lhs_item_ids), class = "AsIs")
  ru$rhs_item_ids <- structure(remap(ru$rhs_item_ids), class = "AsIs")
  it <- it[match(used, it$item_id), , drop = FALSE]
  it$item_id <- seq_along(used) - 1L
  rownames(it) <- rownames(ru) <- NULL
  list(items = it, rules = ru)
}
parsed_all <- merge_parsed(parsed_rings)

# --- 4) Pick one Ring value “fairly” ----------------------------------------
# Build one label per rule’s RHS (ignoring order), then choose among Rings=*
rhs_all <- niarules::rhs_label_per_rule(parsed_all, ignore_order = TRUE)
# keep only pure Rings=K labels
is_rings_label <- grepl("^Rings\\s*=\\s*\\d+$", rhs_all)
rhs_labels <- rhs_all[is_rings_label]
stopifnot(length(rhs_labels) > 0)

# Three pick strategies (uncomment exactly one):
# 1) most frequent Rings value:
rhs_choice <- names(sort(table(rhs_labels), decreasing = TRUE))[1]
# 2) median by rule count:
# tab <- sort(table(rhs_labels)); rhs_choice <- names(tab)[ceiling(length(tab)/2)]
# 3) median “branchiness”:
# b <- tapply(parsed_all$rules$antecedent_length[is_rings_label], rhs_labels, mean)
# rhs_choice <- names(sort(b))[ceiling(length(b)/2)]

parsed_one <- niarules::filter_parsed_by_rhs(parsed_all, rhs_choice, ignore_order = TRUE)
layout     <- niarules::build_coral_plots(parsed_one, lhs_sort_metric = "confidence")

# --- 5) Render (your settings) ----------------------------------------------
dom <- niarules::metric_domains(layout$edges)

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  legend      = TRUE,
  legend_style= "grouped",
  legend_pos  = "inside_right",
  legend_cex  = 1.15,
  
  label_mode  = "interval_short",
  max_labels  = 0,
  theme       = "flat",
  
  node_color_by     = "type",
  node_gradient     = c("#aa7bb6", "#aaf7f7", "#aa191c"),
  node_gradient_map = "hash",
  
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1, 6),      edge_width_domain  = dom$support,
  
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6", "#f7f7f7", "#d7191c"),
  edge_color_domain    = dom$lift,
  
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.2, 1),
  edge_alpha_domain    = dom$confidence,
  
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)
