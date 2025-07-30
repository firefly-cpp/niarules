#' Build radial‐plot data directly from niarules output
#'
#' TODO: full description here
#'
#' @param arules TODO
#' @return TODO
#' 
#' @importFrom dplyr mutate select group_by arrange ungroup
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom niarules write_association_rules_to_csv buildRadialPlots
#' @importFrom utils read.csv
#' @export
build_radial_plots <- function(arules) {
  # write to a short‐lived CSV and arrange for cleanup
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  
  niarules::write_association_rules_to_csv(
    arules,
    file           = tmp_csv,
    is_time_series = FALSE
  )
  
  # read it back in as a plain data.frame
  rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  
  # pivot to the wide format buildRadialPlots() expects
  wdf <- rules_df %>%
    mutate(
      rule_id = row_number(),
      lhs     = gsub("^\\{|\\}$", "", Antecedent),
      rhs     = gsub("^\\{|\\}$", "", Consequence)
    ) %>%
    separate_rows(lhs, sep = ",") %>%
    mutate(
      from = trimws(lhs),
      to   = trimws(rhs)
    ) %>%
    select(
      from, to, rule_id,
      support    = Support,
      confidence = Confidence,
      lift       = Fitness
    ) %>%
    group_by(rule_id) %>%
    arrange(from, .by_group = TRUE) %>%
    mutate(
      item_idx          = row_number(),
      antecedent_length = n()
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols      = c(rule_id, support, confidence, lift, to, antecedent_length),
      names_from   = item_idx,
      values_from  = from,
      names_prefix = "lhs_"
    ) %>%
    rename(rhs = to)
  
  n_plots   <- length(unique(wdf$rhs))
  grid_size <- ceiling(sqrt(n_plots))
  layout <- niarules::buildRadialPlots(wdf, grid_size)
  
  list(
    nodes     = layout$nodes,
    edges     = layout$edges,
    grid_size = grid_size
  )
}