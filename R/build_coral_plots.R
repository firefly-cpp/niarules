#' @title Build Data for Coral Plot Visualization from Association Rules
#'
#' @description
#' Converts an \code{arules}-like object into a structured list containing nodes, edges, and grid size
#' suitable for rendering with coral plot functions (e.g., \code{render_coral_rgl}).
#' Internally, this function serializes the rules to a temporary CSV file, processes them into
#' wide format, and calls \code{niarules::buildCoralPlots()} to compute layout geometry.
#'
#' @param arules An object containing association rules, typically of class \code{rules} from the
#'   \pkg{arules} or \pkg{niarules} package.
#'
#' @return A list with three components:
#' \describe{
#'   \item{\code{nodes}}{A data frame containing node positions, IDs, item names, and radii.}
#'   \item{\code{edges}}{A data frame of edges with coordinates, widths, and colors.}
#'   \item{\code{grid_size}}{An integer representing the layout grid dimension.}
#' }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Writes the input rules to a temporary CSV file using \code{niarules::write_association_rules_to_csv()}.
#'   \item Reads the CSV back in as a standard R data frame.
#'   \item Transforms the data into a wide format with one row per rule and columns \code{lhs_1}, \code{lhs_2}, etc.
#'   \item Computes the number of unique plots needed (based on consequents) and determines a square grid size.
#'   \item Calls \code{niarules::buildCoralPlots()} to compute the layout.
#' }
#'
#' This transformation is necessary to bridge the format expected by the layout algorithm
#' with the format used in rule mining packages like \pkg{arules}.
#'
#' @importFrom dplyr mutate select group_by arrange ungroup
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom niarules write_association_rules_to_csv buildCoralPlots
#' @importFrom utils read.csv
#' @export
build_coral_plots <- function(arules) {
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
  
  # pivot to the wide format buildCoralPlots() expects
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
  layout <- niarules::buildCoralPlots(wdf, grid_size)
  
  list(
    nodes     = layout$nodes,
    edges     = layout$edges,
    grid_size = grid_size
  )
}