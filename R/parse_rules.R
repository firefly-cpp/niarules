#' Internal: normalize metric & LHS/RHS column names
#' @keywords internal
#' @noRd
.normalize_metric_columns <- function(df) {
  # canonical names we want downstream for R-side code:
  #   support, confidence, lift  (all lower-case)
  alias_map <- list(
    support    = c("Support", "support", "supp"),
    confidence = c("Confidence", "confidence", "conf"),
    lift       = c("Lift", "lift", "Fitness", "fitness")
  )
  # also allow LHS/RHS aliases (normalize to title-case for C++)
  lhs_alias   <- c("Antecedent","antecedent","lhs","LHS")
  rhs_alias   <- c("Consequence","consequence","rhs","RHS")
  
  adopt <- function(canon) {
    al <- alias_map[[canon]]
    hit <- al[al %in% names(df)]
    if (length(hit)) {
      if (!canon %in% names(df)) df[[canon]] <- df[[hit[1L]]]
      if (length(hit) > 1L) {
        for (h in hit[-1L]) {
          # only warn if both exist and disagree
          if (!all(is.na(df[[h]]) | is.na(df[[canon]]) | df[[canon]] == df[[h]])) {
            if (isTRUE(getOption("niarules.warn_alias_conflicts", FALSE))) {
              warning(sprintf("Conflicting values for '%s' and '%s'; using '%s'.", hit[1L], h, canon),
                      call. = FALSE)
            }
          }
        }
      }
    }
  }
  
  adopt("support"); adopt("confidence"); adopt("lift")
  
  # Normalize LHS/RHS to title-case expected by C++
  if (!"Antecedent" %in% names(df)) {
    hit <- lhs_alias[lhs_alias %in% names(df)]
    if (length(hit)) df[["Antecedent"]] <- df[[hit[1L]]]
  }
  if (!"Consequence" %in% names(df)) {
    hit <- rhs_alias[rhs_alias %in% names(df)]
    if (length(hit)) df[["Consequence"]] <- df[[hit[1L]]]
  }
  
  # Bridge lower-case canonicals back to C++ title-case columns:
  if (!"Support"    %in% names(df) && "support"    %in% names(df)) df[["Support"]]    <- df[["support"]]
  if (!"Confidence" %in% names(df) && "confidence" %in% names(df)) df[["Confidence"]] <- df[["confidence"]]
  if (!"Fitness"    %in% names(df) && "lift"       %in% names(df)) df[["Fitness"]]    <- df[["lift"]]
  
  df
}

#' @title Parse association rules into a reusable, layout-agnostic structure
#'
#' @description
#' Converts association rules into a normalized representation for downstream
#' layout/rendering. Accepts either:
#' - a `data.frame` with **required** columns
#'   `Antecedent`, `Consequence`, `Support`, `Confidence`, `Fitness`, or
#' - a native `niarules` rules object (which is exported to CSV internally via
#'   `niarules::write_association_rules_to_csv()` and then parsed).
#'
#' The output separates **items** from **rules** and uses stable **0-based**
#' item identifiers suitable for cross-language use.
#'
#' @param arules A `data.frame` with columns
#'   `Antecedent`, `Consequence`, `Support`, `Confidence`, `Fitness`,
#'   **or** a `niarules` rules object.
#'
#' @details
#' **Input requirements**
#' - `Antecedent`, `Consequence`: character encodings of itemsets per rule.
#' - `Support`, `Confidence`, `Fitness`: numeric metrics; `Fitness` is interpreted
#'   as the **lift-like** metric and is exposed as `lift` in the returned `rules`.
#'
#' When `arules` is not a `data.frame`, the function requires the **niarules**
#' package at runtime to serialize the rules to CSV. Missing required columns
#' trigger an error.
#'
#' **Output schema**
#' - `items` (`data.frame`):
#'   `item_id` (integer, **0-based**), `label`, `feature`, `kind`,
#'   `category_value`, `lo`, `hi`, `incl_low`, `incl_high`, `op`,
#'   `label_long`, `label_short`.
#' - `rules` (`data.frame`):
#'   `rule_id`, `support`, `confidence`, `lift`,
#'   `lhs_item_ids` (list of integer vectors; **0-based ids**),
#'   `rhs_item_ids` (list of integer vectors; **0-based ids**),
#'   `antecedent_length`, `consequent_length`.
#'
#' **Indexing note**
#' `item_id` values are **0-based** for stability across languages. In R, convert
#' to 1-based with `items$item_id + 1L` if needed.
#'
#' @return
#' A list with components:
#' - `items`: `data.frame` describing unique items,
#' - `rules`: `data.frame` describing association rules.
#'
#' @importFrom utils read.csv
#' @export
parse_rules <- function(arules = NULL) {
  required_cols <- c("Antecedent","Consequence","Support","Confidence","Fitness")
  
  if (is.data.frame(arules)) {
    rules_df <- arules
  } else {
    if (!requireNamespace("niarules", quietly = TRUE)) {
      stop("parse_rules(): this code path requires the 'niarules' package. Install it first.")
    }
    tmp_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_csv), add = TRUE)
    niarules::write_association_rules_to_csv(arules, file = tmp_csv, is_time_series = FALSE)
    rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  }
  
  rules_df <- .normalize_metric_columns(rules_df)
  
  missing <- setdiff(required_cols, names(rules_df))
  if (length(missing)) {
    stop(sprintf("parse_rules(): missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  
  parsed <- parse_rules_cpp(rules_df)
  
  # add S3 class for downstream generics (metric_domains, etc.)
  class(parsed) <- unique(c("parsed", class(parsed)))
  parsed
}
