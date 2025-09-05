library(niarules)
library(dplyr)

set.seed(1248)

data_raw <- niarules::read_dataset(system.file("extdata","Abalone.csv", package = "niarules"))
data_raw$Rings <- factor(as.integer(data_raw$Rings))

features <- niarules::extract_feature_info(data_raw)
d        <- niarules::problem_dimension(features, is_time_series = FALSE)

mine_de_short <- function(seed, np = 18L, nfes = 360L, f = 0.6, cr = 0.9) {
  req <- c("Antecedent","Consequence","Support","Confidence","Fitness")
  empty <- as.data.frame(setNames(replicate(length(req)+1, numeric(0), FALSE),
                                  c(req, "LHS_len")))
  
  # helper: flatten native niarules-list -> data.frame
  to_df <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    # if it's the full DE result, take $arules
    if (is.list(x) && !is.null(x$arules)) x <- x$arules
    # now expect x to be a list-of-rule-objects
    if (is.list(x) && !is.data.frame(x)) {
      tmp <- tempfile(fileext = ".csv")
      niarules::write_association_rules_to_csv(x, tmp)
      return(utils::read.csv(tmp, check.names = FALSE, stringsAsFactors = FALSE))
    }
    NULL
  }
  
  run_once <- function(np, nfes, f, cr) {
    de <- niarules::differential_evolution(
      d = d, np = np, f = f, cr = cr, nfes = nfes,
      features = features, data = data_raw, is_time_series = FALSE
    )
    df <- to_df(de)
    if (is.null(df) || !all(c("Antecedent","Consequence") %in% names(df))) return(NULL)
    
    # standardize expected columns
    if (!"Fitness" %in% names(df) && "Lift" %in% names(df)) {
      names(df)[match("Lift", names(df))] <- "Fitness"
    }
    need <- setdiff(req, names(df))
    if (length(need)) return(NULL)
    
    lhs <- sub("^\\{(.*)\\}$", "\\1", df$Antecedent)
    len <- ifelse(nzchar(lhs), lengths(strsplit(lhs, "\\s*,\\s*")), 0L)
    transform(df[req], LHS_len = as.integer(len))
  }
  
  out <- tryCatch(run_once(np, nfes, f, cr), error = function(e) NULL)
  if (is.null(out)) {
    message(sprintf("seed %d → 0 rules; retrying with np=%d nfes=%d F=%.2f CR=%.2f",
                    seed, np+4L, as.integer(nfes*1.5), 0.70, 0.95))
    out <- tryCatch(run_once(np+4L, as.integer(nfes*1.5), 0.70, 0.95), error = function(e) NULL)
  }
  if (is.null(out)) {
    message(sprintf("seed %d failed twice", seed))
    empty
  } else {
    message(sprintf("seed %d → %d rules (%d with LHS≥2)",
                    seed, nrow(out), sum(out$LHS_len >= 2)))
    out
  }
}

seeds <- 1:8
library(future.apply)
plan(multisession, workers = max(1, parallel::detectCores() - 1))
harvest_list <- future_lapply(seeds, mine_de_short, future.seed = TRUE)
plan(sequential)

harvest <- dplyr::bind_rows(harvest_list)

# continue as before
harvest <- dplyr::distinct(harvest, Antecedent, Consequence, .keep_all = TRUE)
harvest <- subset(harvest, LHS_len >= 2)
stopifnot(nrow(harvest) > 0L)

write.csv(harvest, "harvested_3.csv")

# --- parse everything ------------------------------------------------------
parsed <- niarules::parse_rules(harvest)

# --- restrict to singleton RHS where the consequent feature is "Rings" ----
items <- parsed$items
rules <- parsed$rules

# get the single RHS item-id per rule (or NA if not singleton)
rhs_single_id <- vapply(
  rules$rhs_item_ids,
  function(v) if (length(v) == 1L) as.integer(v[[1]]) else NA_integer_,
  integer(1)
)
is_single <- !is.na(rhs_single_id)

# map item_id -> feature name
feat_by_id <- setNames(as.character(items$feature), as.character(items$item_id))

target_feature <- "Rings"
is_target <- is_single & tolower(feat_by_id[as.character(rhs_single_id)]) == tolower(target_feature)

eligible_idx <- which(is_target)
stopifnot(length(eligible_idx) > 0L)  # if this fails, see the debug snippet below

# labels only for the eligible rules (use your helper to build strings)
rhs_labels <- niarules::rhs_label_per_rule(parsed, ignore_order = TRUE)
rhs_labels_eligible <- rhs_labels[eligible_idx]

# --- pick ONE Rings value (uncomment one policy) --------------------------
# 1) most frequent Rings label
rhs_choice <- names(sort(table(rhs_labels_eligible), decreasing = TRUE))[1]

# 2) median by rule count
# tab <- sort(table(rhs_labels_eligible))
# rhs_choice <- names(tab)[ceiling(length(tab)/2)]

# 3) median branchiness among eligible only
# b <- tapply(rules$antecedent_length[eligible_idx], rhs_labels_eligible, mean)
# rhs_choice <- names(sort(b))[ceiling(length(b)/2)]

# --- build that coral ------------------------------------------------------
parsed_one <- niarules::filter_parsed_by_rhs(parsed, rhs_choice, ignore_order = TRUE)
layout     <- niarules::build_coral_plots(parsed_one, lhs_sort_metric = "confidence")

# domains & render (unchanged) ---------------------------------------------
dom <- niarules::metric_domains(layout$edges)

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  legend = TRUE, legend_style = "grouped", legend_pos = "inside_right", legend_cex = 1.15,
  label_mode = "interval_short", max_labels = 0, theme = "flat",
  node_color_by = "type", node_gradient = c("#aa7bb6", "#aaf7f7", "#aa191c"), node_gradient_map = "hash",
  edge_width_metric = "support", edge_width_transform = "sqrt", edge_width_range = c(1, 6),
  edge_width_domain = dom$support,
  edge_color_metric = "lift", edge_color_transform = "sqrt",
  edge_gradient = c("#2c7bb6", "#f7f7f7", "#d7191c"), edge_color_domain = dom$lift,
  edge_alpha_metric = "confidence", edge_alpha_range = c(0.2, 1), edge_alpha_domain = dom$confidence,
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)