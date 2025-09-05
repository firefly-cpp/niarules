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

# ----------------------- Config ------------------------------
set.seed(1248)

# Data
data_raw <- niarules::read_dataset(system.file("extdata","Abalone.csv", package = "niarules"))
data_raw$Rings <- factor(as.integer(data_raw$Rings))

# Targets (integers present in the data)
RINGS_VALUES <- sort(unique(as.integer(data_raw$Rings)))

# Quota per integer RHS (start small for quick testing)
QUOTA_PER_VALUE <- 2L

# Mining params (short runs; tweak as needed)
NP        <- 18L
NFES      <- 360L
F_MUT     <- 0.60
CR        <- 0.90

# Parallel + batch setup
WORKERS        <- max(1L, parallel::detectCores() - 1L)
SEEDS_PER_BATCH<- 8L                           # parallel jobs per batch
MAX_BATCHES    <- 200L                         # hard stop
CHECKPOINT_CSV <- "harvest_rules.csv"

# Acceptance filters
LHS_MIN_LEN    <- 2L
MIN_CONF       <- 0.00                         # set higher if you want
MIN_SUPP       <- 0.00                         # set higher if you want

# Bootstrapping
BOOTSTRAP_SIZE <- nrow(data_raw)               # same size as dataset
POS_FRACTION   <- 0.67                         # ~ 2/3 from the target Rings=v

# ---- Rings mapping behavior for numerical intervals on RHS ----
# "split"   → replicate a rule for every integer overlapped by the interval
# "nearest" → map to the nearest integer (midpoint)
# "strict"  → only keep if span <= 1 and midpoint is within tolerance of an integer
RINGS_MAP_MODE   <- "split"   # "split" | "nearest" | "strict"
RINGS_NEAREST_TOL<- 0.5       # used in "strict"

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
    sprintf("%s∈[%.6g,%.6g]", p$name, b1, b2)
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

# ------- De-duplication key (keep your existing one if you like) -------
rule_key <- function(lhs_str, rhs_str) {
  lhs_core  <- sub("^\\{(.*)\\}$", "\\1", as.character(lhs_str))
  lhs_items <- if (nzchar(lhs_core)) sort(strsplit(lhs_core, "\\s*,\\s*")[[1]]) else character(0)
  paste0(paste(lhs_items, collapse = "|"), " => ", gsub("\\s+", "", as.character(rhs_str)))
}

# Extract RHS integer if the consequence is Rings=integer
#rhs_integer_if_rings <- function(cons_str) {
#  cs <- gsub("[{}\\s]", "", as.character(cons_str))
#  # typical patterns: "Rings=9"  or possibly "Rings==9"
#  m <- regexec("(?i)^Rings=+([0-9]+)$", cs, perl = TRUE)
#  reg <- regmatches(cs, m)
#  v <- suppressWarnings(as.integer(vapply(reg, function(z) if (length(z) == 2) z[2] else NA_character_, "")))
#  ifelse(is.na(v), NA_integer_, v)
#}

# Run one short DE on a given (bootstrapped) dataset
mine_de_short <- function(df_boot, seed,
                          np = NP, nfes = NFES, f = F_MUT, cr = CR) {
  # set the RNG for reproducibility per worker
  set.seed(seed)
  
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
        assign(rule_key(old$Antecedent[i], old$Consequence[i]), TRUE, envir = keys)
      }
    }
  }
}

counts <- setNames(sapply(RINGS_VALUES, function(v) nrow(kept[[as.character(v)]] %||% data.frame())), RINGS_VALUES)

# simple utility
below_quota <- function() {
  setdiff(RINGS_VALUES[counts[RINGS_VALUES] < quota[as.character(RINGS_VALUES)]], integer(0))
}

batch <- 0L
while (length(below_quota()) && batch < MAX_BATCHES) {
  batch <- batch + 1L
  
  # pick the Rings value with largest deficit
  deficits <- quota - counts
  target_v <- as.integer(names(deficits)[which.max(deficits)])
  target_v <- if (length(target_v)) target_v else below_quota()[1]
  
  # build a class-balanced bootstrap
  df_boot <- bootstrap_for_value(data_raw, v = target_v)
  
  # run a parallel batch of short DE runs
  seeds <- total_seeds_tried + seq_len(SEEDS_PER_BATCH)
  res_list <- future_lapply(
    seeds,
    function(s) mine_de_short(df_boot, seed = s),
    future.packages = c("niarules")
  )
  total_seeds_tried <- total_seeds_tried + length(seeds)
  
  # combine and filter to RHS == Rings=target_v
  cand <- bind_rows(res_list)
  cat("cand rules this batch:", nrow(cand), "\n")
  if (nrow(cand)) {
    tab <- sort(table(cand$rhs_name), decreasing = TRUE)
    cat("Top RHS:", paste(head(paste0(names(tab), "=", as.integer(tab)), 8), collapse = " | "), "\n")
  }
  if (nrow(cand)) {
    #cand$v <- rhs_integer_if_rings(cand$Consequence)
    cand <- cand %>%
      dplyr::filter(!is.na(v),
                    v == target_v,
                    LHS_len >= LHS_MIN_LEN,
                    Support >= MIN_SUPP,
                    Confidence >= MIN_CONF)
    
    # de-duplicate and keep only new ones
    if (nrow(cand)) {
      cand$key <- mapply(rule_key, cand$Antecedent, cand$Consequence, USE.NAMES = FALSE)
      is_new <- !vapply(cand$key, exists, logical(1), envir = keys)
      new_rules <- cand[is_new, setdiff(names(cand), c("key")), drop = FALSE]
      
      # register keys
      if (nrow(new_rules)) {
        invisible(mapply(function(k) assign(k, TRUE, envir = keys), cand$key[is_new]))
        kept_key <- as.character(target_v)
        kept[[kept_key]] <- bind_rows(kept[[kept_key]], new_rules)
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
  status <- paste(sprintf("%d:%d", RINGS_VALUES, counts), collapse = "  ")
  msg <- sprintf("[batch %03d] seeds tried: %d | targeting Rings=%d | rules per ring: %s",
                 batch, total_seeds_tried, target_v, status)
  message(msg); flush.console()
}

# Final summary
message("Done.")
message(sprintf("Total seeds tried: %d", total_seeds_tried))
final_counts <- setNames(sapply(RINGS_VALUES, function(v) nrow(kept[[as.character(v)]] %||% data.frame())), RINGS_VALUES)
message(sprintf("Final rules per ring: %s",
                paste(sprintf("%d:%d", RINGS_VALUES, final_counts), collapse = "  ")))
message(sprintf("Checkpoint written to: %s", CHECKPOINT_CSV))
