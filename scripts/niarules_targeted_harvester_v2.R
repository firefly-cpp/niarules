# targeted_harvester.R
# -------------------------------------------------------------
# Anytime targeted miner wrapper around niarules::differential_evolution
# - class-balanced bootstraps per Rings = v
# - parallel short runs, periodic logging, CSV checkpoints
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(niarules)
  library(dplyr)
  library(stringr)
  library(future.apply)
  library(readr)
})

# null-coalescing helper (used below)
`%||%` <- function(a, b) if (!is.null(a)) a else b

Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly=TRUE)) RhpcBLASctl::blas_set_num_threads(1)

# ----------------------- Config ------------------------------
# Reproducible, parallel-safe RNG
RNGkind("L'Ecuyer-CMRG")
MASTER_SEED <- 1248
set.seed(MASTER_SEED)

# Data
data_raw <- niarules::read_dataset(system.file("extdata","Abalone.csv", package = "niarules"))
data_raw$Rings <- factor(as.integer(data_raw$Rings))
# After: data_raw$Rings <- factor(as.integer(data_raw$Rings))
ring_freq <- sort(table(as.integer(data_raw$Rings)), decreasing = TRUE)

# Focus on frequent rings first – pick one of these knobs if you want:
TOP_K_RINGS   <- 6      # e.g., keep top 8 most common ring values
MIN_RING_COUNT<- NULL   # or set a minimum count, e.g., 80

if (!is.null(TOP_K_RINGS)) {
  keep_vals <- as.integer(names(ring_freq)[seq_len(min(TOP_K_RINGS, length(ring_freq)))])
} else if (!is.null(MIN_RING_COUNT)) {
  keep_vals <- as.integer(names(ring_freq[ring_freq >= MIN_RING_COUNT]))
} else {
  keep_vals <- as.integer(names(ring_freq))
}
RINGS_VALUES <- sort(unique(keep_vals))

cat("Targeting ring values (by frequency):", paste(RINGS_VALUES, collapse=", "), "\n")

# Quota per integer RHS (start small for quick testing)
QUOTA_PER_VALUE <- 40L

# Mining params (short runs; tweak as needed)
NP        <- 12L
NFES      <- 180L
F_MUT     <- 0.60
CR        <- 0.90

# Parallel + batch setup
WORKERS        <- max(1L, parallel::detectCores() - 1L)
SEEDS_PER_BATCH<- 4L                           # parallel jobs per batch
MAX_BATCHES    <- 200L                         # hard stop
CHECKPOINT_CSV <- "harvest_rules.csv"

# Acceptance filters
LHS_MIN_LEN    <- 2L
MIN_CONF       <- 0.00                         # set higher if you want
MIN_SUPP       <- 0.00                         # set higher if you want

# Bootstrapping
OOTSTRAP_SIZE <- min(nrow(data_raw), 1500L)               # same size as dataset
POS_FRACTION   <- 0.9                        # ~ 2/3 from the target Rings=v

# ---- Rings mapping behavior for numerical intervals on RHS ----
# "split"   → replicate a rule for every integer overlapped by the interval
# "nearest" → map to the nearest integer (midpoint)
# "strict"  → only keep if span <= 1 and midpoint is within tolerance of an integer
RINGS_MAP_MODE   <- "split"   # "split" | "nearest" | "strict"
RINGS_NEAREST_TOL<- 0.5       # used in "strict"

batch_times <- numeric(0)

# -------------------------------------------------------------
# Helpers
# -------------------------------------------------------------

# Bootstrap with over-representation of a particular Rings value
bootstrap_for_value <- function(df, v, size = BOOTSTRAP_SIZE, pos_frac = POS_FRACTION) {
  idx_pos <- which(as.integer(df$Rings) == v)
  idx_neg <- which(as.integer(df$Rings) != v)
  if (!length(idx_pos)) return(df[sample.int(nrow(df), size, replace = TRUE), , drop = FALSE])
  
  n_pos <- max(1L, round(pos_frac * size))
  n_neg <- max(1L, size - n_pos)
  
  samp <- c(sample(idx_pos, n_pos, replace = TRUE),
            sample(idx_neg, n_neg, replace = TRUE))
  df[samp, , drop = FALSE]
}

# ------- Predicate → readable string (for pretty keys/logging) -------
pred_to_string <- function(p) {
  if (identical(p$type, "categorical")) {
    val <- if (is.null(p$value) || identical(p$value, "EMPTY")) NA_character_ else as.character(p$value)
    if (!is.na(val) && nzchar(val)) sprintf("%s==%s", p$name, val) else sprintf("%s", p$name)
  } else {
    b1 <- suppressWarnings(as.numeric(p$border1))
    b2 <- suppressWarnings(as.numeric(p$border2))
    PRED_DIGITS <- 4
    sprintf("%s∈[%.*f,%.*f]", p$name, PRED_DIGITS, b1, PRED_DIGITS, b2)
  }
}

lhs_to_string <- function(ant_list) {
  if (is.null(ant_list) || !length(ant_list)) return("{}")
  items <- vapply(ant_list, pred_to_string, character(1))
  items <- sort(items)
  paste0("{", paste(items, collapse = ", "), "}")
}

rhs_to_string <- function(q) pred_to_string(q)

# ------- Helpers for mapping 'Rings' consequents to integer(s) -------
parse_int_from_rings_name <- function(name) {
  # accepts "Rings (20)", "Rings(20)", "Rings 20"
  m <- regexec("(?i)^\\s*Rings\\s*\\(?\\s*([0-9]+)\\s*\\)?\\s*$", as.character(name), perl = TRUE)
  reg <- regmatches(name, m)
  if (length(reg) && length(reg[[1]]) == 2) as.integer(reg[[1]][2]) else NA_integer_
}

map_rhs_to_ring_integers <- function(q, mode = RINGS_MAP_MODE) {
  nm <- as.character(q$name)
  if (!grepl("(?i)^\\s*Rings", nm)) return(integer(0))
  
  if (identical(q$type, "categorical")) {
    # Try the value first; fall back to parsing from the name ("Rings (20)")
    val <- suppressWarnings(as.integer(q$value))
    if (!is.na(val)) return(val)
    nfromname <- parse_int_from_rings_name(nm)
    if (!is.na(nfromname)) return(nfromname)
    return(integer(0))
  }
  
  # numerical → interval
  b1 <- suppressWarnings(as.numeric(q$border1))
  b2 <- suppressWarnings(as.numeric(q$border2))
  if (!is.finite(b1) || !is.finite(b2)) return(integer(0))
  if (b2 < b1) { tmp <- b1; b1 <- b2; b2 <- tmp }
  span <- b2 - b1
  mid  <- (b1 + b2) / 2
  
  if (mode == "nearest") {
    return(as.integer(round(mid)))
  } else if (mode == "strict") {
    if (span <= 1.0) {
      n <- as.integer(round(mid))
      if (abs(mid - n) <= RINGS_NEAREST_TOL) return(n)
    }
    return(integer(0))
  } else { # "split" (default)
    lo <- floor(b1 + 1e-9)
    hi <- ceiling(b2 - 1e-9)
    cand <- seq(lo, hi)
    cand <- cand[cand >= floor(b1) & cand <= ceiling(b2)]
    unique(as.integer(cand))
  }
}

# ------- Flatten niarules::differential_evolution() result -------
flatten_de_rules <- function(de_obj) {
  arr <- de_obj$arules
  if (is.null(arr) || !length(arr)) return(data.frame())
  
  out_list <- list(); k <- 0L
  
  for (r in arr) {
    lhs_str <- lhs_to_string(r$antecedent)
    lhs_len <- if (identical(lhs_str, "{}")) 0L else length(r$antecedent)
    supp    <- suppressWarnings(as.numeric(r$support))
    conf    <- suppressWarnings(as.numeric(r$confidence))
    fit     <- suppressWarnings(as.numeric(r$fitness))
    
    cons <- r$consequent
    if (is.null(cons) || !length(cons)) next
    
    for (q in cons) {
      rhs_str  <- rhs_to_string(q)
      rhs_name <- as.character(q$name)
      v_ints   <- map_rhs_to_ring_integers(q)
      
      if (length(v_ints) == 0) {
        k <- k + 1L
        out_list[[k]] <- data.frame(
          Antecedent = lhs_str, Consequence = rhs_str,
          Support = supp, Confidence = conf, Fitness = fit,
          LHS_len = lhs_len, rhs_name = rhs_name, v = NA_integer_,
          stringsAsFactors = FALSE
        )
      } else {
        for (v in v_ints) {
          k <- k + 1L
          out_list[[k]] <- data.frame(
            Antecedent = lhs_str, Consequence = rhs_str,
            Support = supp, Confidence = conf, Fitness = fit,
            LHS_len = lhs_len, rhs_name = rhs_name, v = as.integer(v),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(out_list)) return(data.frame())
  dplyr::bind_rows(out_list)
}

# ------- de-duplication
split_outside_brackets <- function(s) {
  if (!nzchar(s)) return(character(0))
  res <- character(0); buf <- ""; depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch %in% c("[","(")) depth <- depth + 1L
    else if (ch %in% c("]",")")) depth <- max(0L, depth - 1L)
    if (depth == 0L && ch %in% c(",", "&")) {
      part <- trimws(buf); if (nzchar(part)) res <- c(res, part); buf <- ""
    } else buf <- paste0(buf, ch)
  }
  part <- trimws(buf); if (nzchar(part)) res <- c(res, part)
  res
}

lhs_key <- function(lhs_str) {
  lhs_core  <- sub("^\\{(.*)\\}$", "\\1", as.character(lhs_str))
  items <- if (nzchar(lhs_core)) split_outside_brackets(lhs_core) else character(0)
  paste(sort(items[nzchar(items)]), collapse = "|")
}

rule_key <- function(lhs_str, v) {
  paste0(lhs_key(lhs_str), " | Rings=", v)
}

# Run one short DE on a given (bootstrapped) dataset
mine_de_short <- function(df_boot, seed,
                          np = NP, nfes = NFES, f = F_MUT, cr = CR) {
  
  features <- niarules::extract_feature_info(df_boot)
  d        <- niarules::problem_dimension(features, is_time_series = FALSE)
  
  out <- tryCatch(
    niarules::differential_evolution(
      d          = d,
      np         = np,
      f          = f,
      cr         = cr,
      nfes       = nfes,
      features   = features,
      data       = df_boot,
      is_time_series = FALSE
    ),
    error = function(e) {
      message("DE error (seed=", seed, "): ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(out)) return(data.frame())
  flatten_de_rules(out)
}


# -------------------------------------------------------------
# Targeted anytime harvesting
# -------------------------------------------------------------

# Keep resource use conservative while we stabilize
WORKERS <- min(WORKERS, 4L)             # cap workers (bump later if stable)
options(future.globals.maxSize = 1e9)   # 1 GB; raise if needed
# Clean up from previous runs (avoids zombie workers)
if ("ClusterRegistry" %in% ls(getNamespace("future"))) {
  try(future:::ClusterRegistry("stop"), silent = TRUE)
}
plan(multisession, workers = WORKERS)

quota <- setNames(rep(QUOTA_PER_VALUE, length(RINGS_VALUES)), RINGS_VALUES)
kept  <- list()                              # list(value -> data.frame of rules)
keys  <- new.env(parent = emptyenv())        # fast set for de-dup keys
total_seeds_tried <- 0L

# load checkpoint if exists
if (file.exists(CHECKPOINT_CSV)) {
  cat("Checkpoint found:", CHECKPOINT_CSV, "→ loading\n")
  old <- suppressMessages(readr::read_csv(CHECKPOINT_CSV, show_col_types = FALSE))
  if (nrow(old)) {
    if (!("v" %in% names(old))) {
      # fall back: try to recover v from rhs name/text if CSV was from an older run
      old$v <- NA_integer_
      # try "Rings (20)" in rhs_name first
      if ("rhs_name" %in% names(old)) {
        guess <- vapply(old$rhs_name, parse_int_from_rings_name, integer(1))
        old$v[is.na(old$v)] <- guess[is.na(old$v)]
      }
      # as a last resort, try parsing from Consequence string (less robust)
      if ("Consequence" %in% names(old)) {
        nm <- as.character(old$Consequence)
        guess2 <- vapply(nm, function(s) {
          m <- regexec("(?i)Rings.*?([0-9]+)", s, perl = TRUE)
          rg <- regmatches(s, m)[[1]]
          if (length(rg) == 2) as.integer(rg[2]) else NA_integer_
        }, integer(1))
        old$v[is.na(old$v)] <- guess2[is.na(old$v)]
      }
    }
    old <- old[!is.na(old$v), , drop = FALSE]
    if (nrow(old)) {
      split(old, old$v) |>
        lapply(function(df) { kept[[as.character(df$v[1])]] <<- df; invisible(NULL) })
      # rebuild de-dup keys
      for (i in seq_len(nrow(old))) {
        assign(rule_key(old$Antecedent[i], old$v[i]), TRUE, envir = keys)
      }
    }
  }
}

counts <- setNames(sapply(RINGS_VALUES, function(v) nrow(kept[[as.character(v)]] %||% data.frame())), RINGS_VALUES)

# simple utility
below_quota <- function() {
  need <- RINGS_VALUES[ counts[as.character(RINGS_VALUES)] < quota[as.character(RINGS_VALUES)] ]
  setdiff(need, integer(0))
}

batch <- 0L
while (length(below_quota()) && batch < MAX_BATCHES) {
  batch_start <- Sys.time()
  
  batch <- batch + 1L
  
  # pick the Rings value with largest deficit
  deficits <- quota - counts
  candidates <- as.integer(names(deficits)[deficits > 0])
  if (length(candidates)) {
    # choose the candidate with the highest dataset frequency
    freqs <- as.integer(ring_freq[as.character(candidates)])
    target_v <- candidates[which.max(freqs)]
  } else {
    target_v <- NA_integer_
  }
  
  # build a class-balanced bootstrap
  df_boot <- bootstrap_for_value(data_raw, v = target_v)
  
  # run a parallel batch of short DE runs
  seeds <- total_seeds_tried + seq_len(SEEDS_PER_BATCH)
  res_list <- tryCatch(
    future_lapply(
      seeds,
      function(s) mine_de_short(df_boot, seed = s),
      future.packages = c("niarules"),
      future.seed = TRUE
    ),
    error = function(e) {
      message("Parallel batch failed (", conditionMessage(e), "). Falling back to sequential for this batch.")
      lapply(seeds, function(s) mine_de_short(df_boot, seed = s))
    }
  )
  total_seeds_tried <- total_seeds_tried + length(seeds)
  
  # combine and filter to RHS == Rings=target_v
  cand <- bind_rows(res_list)
  cat("cand rules this batch:", nrow(cand), "\n")
  if (nrow(cand)) {
    vtab <- sort(table(cand$v[!is.na(cand$v) & cand$v %in% RINGS_VALUES]), decreasing = TRUE)
    if (length(vtab)) cat("Rings in batch (pre-filter):", paste(head(paste0(names(vtab), "=", as.integer(vtab)), 12), collapse=" | "), "\n")
  }
  if (nrow(cand)) {
    tab <- sort(table(cand$rhs_name), decreasing = TRUE)
    cat("Top RHS:", paste(head(paste0(names(tab), "=", as.integer(tab)), 8), collapse = " | "), "\n")
  }
  if (nrow(cand)) {
    cand <- cand %>%
      dplyr::filter(!is.na(v),
                    v %in% RINGS_VALUES,
                    LHS_len >= LHS_MIN_LEN,
                    Support >= MIN_SUPP,
                    Confidence >= MIN_CONF)
    
    # de-duplicate and keep only new ones
    if (nrow(cand)) {
      cand$key <- mapply(function(a, v) rule_key(a, v), cand$Antecedent, cand$v, USE.NAMES = FALSE)
      is_new <- !vapply(cand$key, exists, logical(1), envir = keys)
      # keep the key so we can collapse within-batch dups
      new_rules <- cand[is_new, , drop = FALSE]
      
      if (nrow(new_rules)) {
        # collapse any within-batch duplicates (same LHS set + v) by best Fitness/Confidence/Support
        new_rules <- new_rules |>
          dplyr::group_by(key) |>
          dplyr::slice_max(order_by = dplyr::coalesce(Fitness, Confidence, Support),
                           n = 1, with_ties = FALSE) |>
          dplyr::ungroup()
        
        # register keys so future batches recognize these as seen
        invisible(vapply(new_rules$key, function(k) { assign(k, TRUE, envir = keys); TRUE }, logical(1)))
        
        # append to per-ring buckets
        by_v <- split(new_rules, new_rules$v)
        for (vk in names(by_v)) {
          kept[[vk]] <- dplyr::bind_rows(kept[[vk]], by_v[[vk]])
        }
      }
    }
  }
  
  # recompute counts and write checkpoint CSV
  counts <- setNames(sapply(RINGS_VALUES, function(v) nrow(kept[[as.character(v)]] %||% data.frame())), RINGS_VALUES)
  pool <- bind_rows(lapply(names(kept), function(k) kept[[k]]))
  if (nrow(pool)) {
    suppressMessages(write_csv(pool, CHECKPOINT_CSV))
  }
  
  # periodic log
  ordered_counts <- counts[as.character(RINGS_VALUES)]
  status <- paste(sprintf("%d:%d", RINGS_VALUES, ordered_counts), collapse = "  ")
  
  elapsed  <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))
  batch_times <- c(batch_times, elapsed)
  ma5 <- mean(tail(batch_times, 5))
  
  cand_n <- if (exists("cand") && is.data.frame(cand)) nrow(cand) else 0
  cps    <- if (elapsed > 0) cand_n / elapsed else NA
  
  msg <- sprintf("[batch %03d] seeds tried: %d | targeting Rings=%d | rules per ring: %s",
                 batch, total_seeds_tried, target_v, status)
  
  msg <- paste0(
    msg,
    sprintf(" | batch_time=%.1fs | avg_last5=%.1fs | cand/s=%s",
            elapsed, ma5, if (is.finite(cps)) sprintf("%.1f", cps) else "NA")
  )
  
  message(msg); flush.console()
}

# -------------------- POST-STEP: re-score on full data --------------------
# This assumes:
# - full data in: data_raw  (with data_raw$Rings as a factor of integers)
# - harvested rules in: CHECKPOINT_CSV (with columns: Antecedent, Consequence, v, LHS_len, ...)

# --- helpers to evaluate LHS strings on full data ---
split_outside_brackets <- function(s) {
  if (!nzchar(s)) return(character(0))
  res <- character(0); buf <- ""; depth <- 0L
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch %in% c("[","(")) depth <- depth + 1L
    else if (ch %in% c("]",")")) depth <- max(0L, depth - 1L)
    if (depth == 0L && ch %in% c(",", "&")) {
      part <- trimws(buf); if (nzchar(part)) res <- c(res, part); buf <- ""
    } else {
      buf <- paste0(buf, ch)
    }
  }
  part <- trimws(buf); if (nzchar(part)) res <- c(res, part)
  res
}

parse_items <- function(lhs_str) {
  core <- sub("^\\{?\\s*(.*)\\s*\\}?$", "\\1", lhs_str)   # strip optional {}
  if (!nzchar(core)) character(0) else split_outside_brackets(core)
}

mask_for_item <- function(item, df) {
  s <- trimws(item)
  
  # 1) categorical equality: accept "==" or "="
  m1 <- regexec("^([^=∈]+)\\s*={1,2}\\s*(.+)$", s, perl = TRUE)
  rg1 <- regmatches(s, m1)[[1]]
  if (length(rg1) == 3) {
    name  <- trimws(rg1[2]); val <- trimws(rg1[3])
    if (!name %in% names(df)) return(rep(TRUE, nrow(df)))
    col <- df[[name]]
    return(as.character(col) == val)
  }
  
  # 2) numeric interval: Name∈[lo,hi] (inclusive)
  m2 <- regexec("^([^=∈]+)∈\\[\\s*([^,\\]]+)\\s*,\\s*([^\\]]+)\\s*\\]$", s, perl = TRUE)
  rg2 <- regmatches(s, m2)[[1]]
  if (length(rg2) == 4) {
    name <- trimws(rg2[2]); lo <- suppressWarnings(as.numeric(rg2[3])); hi <- suppressWarnings(as.numeric(rg2[4]))
    if (!name %in% names(df) || !is.finite(lo) || !is.finite(hi)) return(rep(TRUE, nrow(df)))
    col <- suppressWarnings(as.numeric(df[[name]]))
    return(!is.na(col) & col >= min(lo,hi) & col <= max(lo,hi))
  }
  
  # unrecognized predicate → neutral (TRUE)
  rep(TRUE, nrow(df))
}

mask_for_lhs <- function(lhs_str, df) {
  items <- parse_items(lhs_str)
  if (!length(items)) return(rep(TRUE, nrow(df)))
  ms <- lapply(items, mask_for_item, df = df)
  Reduce(`&`, ms)
}

# --- compute full-data metrics for a data.frame of rules (has columns: Antecedent, v, ...) ---
rescore_full_metrics <- function(rules_df, full_df) {
  n <- nrow(full_df)
  rings_int <- as.integer(full_df$Rings)
  prior_tab <- table(rings_int)
  pC <- function(v) as.numeric(prior_tab[as.character(v)] %||% 0) / n
  
  # precompute P(C) per distinct v to avoid redoing it
  v_vals <- sort(unique(rules_df$v))
  pC_map <- setNames(vapply(v_vals, pC, numeric(1)), v_vals)
  
  # evaluate rules
  out <- rules_df
  out$Support_full    <- NA_real_
  out$Confidence_full <- NA_real_
  out$Lift_full       <- NA_real_
  
  for (i in seq_len(nrow(rules_df))) {
    lhs <- rules_df$Antecedent[i]
    v   <- rules_df$v[i]
    if (is.na(v)) next
    
    mA <- mask_for_lhs(lhs, full_df)
    mC <- (as.integer(full_df$Rings) == v)
    
    nA  <- sum(mA)
    nC  <- sum(mC)
    nAC <- sum(mA & mC)
    
    supp <- nAC / n
    conf <- if (nA > 0) nAC / nA else NA_real_
    lift <- if (!is.na(conf) && (pC_map[as.character(v)] > 0)) conf / pC_map[as.character(v)] else NA_real_
    
    out$Support_full[i]    <- supp
    out$Confidence_full[i] <- conf
    out$Lift_full[i]       <- lift
  }
  out
}


# convenience wrapper
rescore_file <- function(in_csv, out_csv = NULL, full_df = data_raw) {
  rules_raw <- suppressMessages(readr::read_csv(in_csv, show_col_types = FALSE))
  rescored  <- rescore_full_metrics(rules_raw, full_df)
  out_csv   <- out_csv %||% sub("\\.csv$", "_full.csv", in_csv)
  suppressMessages(readr::write_csv(rescored, out_csv))
  message(sprintf("Re-scored on full data → %s (%d rules)", out_csv, nrow(rescored)))
  invisible(rescored)
}
#rescore_file("harvest_rules.csv")

# --- run the post-step on the current checkpoint ---
if (file.exists(CHECKPOINT_CSV)) {
  rules_raw <- suppressMessages(readr::read_csv(CHECKPOINT_CSV, show_col_types = FALSE))
  if (nrow(rules_raw)) {
    rescored <- rescore_full_metrics(rules_raw, data_raw)
    out_csv  <- sub("\\.csv$", "_full.csv", CHECKPOINT_CSV)
    suppressMessages(readr::write_csv(rescored, out_csv))
    message(sprintf("Re-scored on full data → %s (added: Support_full, Confidence_full, Lift_full)", out_csv))
  } else {
    message("Checkpoint exists but has no rows — skipping full-data re-scoring.")
  }
} else {
  message("No checkpoint CSV found — skipping full-data re-scoring.")
}
# -----------------------------------------------------------------------

# Final summary
message("Done.")
message(sprintf("Total seeds tried: %d", total_seeds_tried))
final_counts <- setNames(sapply(RINGS_VALUES, function(v) nrow(kept[[as.character(v)]] %||% data.frame())), RINGS_VALUES)
message(sprintf("Final rules per ring: %s",
                paste(sprintf("%d:%d", RINGS_VALUES, final_counts), collapse = "  ")))
message(sprintf("Checkpoint written to: %s", CHECKPOINT_CSV))
