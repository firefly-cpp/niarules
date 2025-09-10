.normalize_metric_columns <- function(df) {
  # canonical names we want downstream:
  #   support, confidence, lift  (all lower-case in layout$edges)
  alias_map <- list(
    support    = c("Support", "support", "supp"),
    confidence = c("Confidence", "confidence", "conf"),
    lift       = c("Lift", "lift", "Fitness", "fitness")
  )
  
  # helper: copy first existing alias into canonical name
  adopt <- function(canon) {
    al <- alias_map[[canon]]
    hit <- al[al %in% names(df)]
    if (length(hit)) {
      if (!canon %in% names(df)) df[[canon]] <- df[[hit[1L]]]
      # if both canon and alias exist but disagree, warn
      if (length(hit) > 1L) {
        for (h in hit[-1L]) {
          if (!all(is.na(df[[h]]) | is.na(df[[canon]]) | df[[canon]] == df[[h]])) {
            warning(sprintf("Conflicting values for '%s' and '%s'; using '%s'.", hit[1L], h, canon),
                    call. = FALSE)
          }
        }
      }
    }
  }
  
  adopt("support"); adopt("confidence"); adopt("lift")
  
  # Friendly warnings for legacy columns (soft deprecation)
  if ("Fitness" %in% names(df) && !"lift" %in% names(df)) {
    warning("Column 'Fitness' is accepted for backward compatibility; prefer 'lift'.",
            call. = FALSE)
  }
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
    tmp_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_csv), add = TRUE)
    niarules::write_association_rules_to_csv(arules, file = tmp_csv, is_time_series = FALSE)
    rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  }
  
  rules_df <- .normalize_metric_columns(rules_df)   # ← add this line
  
  missing <- setdiff(required_cols, names(rules_df))
  if (length(missing)) {
    stop(sprintf("parse_rules: missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  
  parsed <- parse_rules_cpp(rules_df)
  if (!is.list(parsed) || !all(c("items","rules") %in% names(parsed))) {
    stop("parse_rules: internal error (unexpected return from C++).", call. = FALSE)
  }
  parsed
}
