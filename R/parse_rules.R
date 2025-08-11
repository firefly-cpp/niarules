#' @title parses association rules into a reusable, layout-agnostic structure
#'
#' @description accepts either:
#' - a data.frame with niarules-like columns (Antecedent, Consequence, Support, Confidence, Fitness), or
#' - a native niarules rules object (written to CSV via niarules::write_association_rules_to_csv)
#'
#' Returns a list with:
#'   - items: data.frame(item_id, label, feature, kind, category_value, lo, hi, incl_low, incl_high, op, label_long, label_short)
#'   - rules: data.frame(rule_id, support, confidence, lift, lhs_item_ids(list<int>), rhs_item_ids(list<int>),
#'                       antecedent_length, consequent_length)
#'   Note: item_id values are 0-based integers (stable across this parsed object).
#'
#' @param arules data.frame | niarules object
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
