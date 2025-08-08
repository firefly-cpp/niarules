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
  
  # helper: split on commas that are OUTSIDE parentheses
  split_outside_parens <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    x <- gsub("^\\{|\\}$", "", x)  # strip outer braces from "{...}"
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    depth <- 0L
    parts <- character(0)
    buf <- character(0)
    for (ch in chars) {
      if (ch == "(") depth <- depth + 1L
      if (ch == ")" && depth > 0L) depth <- depth - 1L
      if (ch == "," && depth == 0L) {
        parts <- c(parts, trimws(paste0(buf, collapse = "")))
        buf <- character(0)
      } else {
        buf <- c(buf, ch)
      }
    }
    parts <- c(parts, trimws(paste0(buf, collapse = "")))
    parts[nzchar(parts)]
  }
  
  # clean feature name from an item label like:
  # "Length (0.63, 0.76)", "Sex = M", "Diameter>=0.4", "Class in {A,B}"
  parse_feature <- function(x) {
    x <- trimws(x)
    # drop everything starting at a parenthesis if present (interval form)
    x <- sub("\\s*\\(.*$", "", x)
    # then drop operator and RHS if present
    sub("\\s*(<=|>=|=|<|>|\\s+in\\s+|%in%).*$", "", x, perl = TRUE)
  }
  
  
  #tmp_csv <- tempfile(fileext = ".csv")
  #on.exit(unlink(tmp_csv), add = TRUE)
  
  #niarules::write_association_rules_to_csv(
  #  arules,
  #  file           = tmp_csv,
  #  is_time_series = FALSE
  #)
  
  #rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  
  #alt (since i dont want to mine all the time)
  rules_df <- utils::read.csv("test.csv", stringsAsFactors = FALSE)
  
  # turn each rule into rows: one per LHS item (paren-aware)
  lhs_rows <- lapply(seq_len(nrow(rules_df)), function(i) {
    lhs_items <- split_outside_parens(rules_df$Antecedent[i])
    if (length(lhs_items) == 0L) lhs_items <- NA_character_
    data.frame(rule_id = i, lhs = lhs_items, stringsAsFactors = FALSE)
  })
  lhs_rows <- do.call(rbind, lhs_rows)
  
  wdf <- lhs_rows %>%
    dplyr::mutate(
      from = lhs,
      to   = gsub("^\\{|\\}$", "", rules_df$Consequence[rule_id]) |> trimws()
    ) %>%
    dplyr::left_join(
      dplyr::mutate(rules_df,
                    rule_id   = dplyr::row_number(),
                    support    = Support,
                    confidence = Confidence,
                    lift       = Fitness
      ) |>
        dplyr::select(rule_id, support, confidence, lift),
      by = "rule_id"
    ) %>%
    dplyr::arrange(rule_id, from) %>%
    dplyr::group_by(rule_id) %>%
    dplyr::mutate(
      item_idx          = dplyr::row_number(),
      antecedent_length = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols      = c(rule_id, support, confidence, lift, to, antecedent_length),
      names_from   = item_idx,
      values_from  = from,
      names_prefix = "lhs_"
    ) %>%
    dplyr::rename(rhs = to)
  
  # ---- Build item_types from labels without breaking on bound-commas ----
  lhs_items <- wdf %>%
    dplyr::select(dplyr::starts_with("lhs_")) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "k", values_to = "item") %>%
    dplyr::filter(!is.na(item)) %>%
    dplyr::pull(item)
  
  all_items <- unique(c(lhs_items, wdf$rhs))
  
  item_types <- data.frame(
    item = all_items,
    type = vapply(all_items, parse_feature, character(1)),
    stringsAsFactors = FALSE
  )
  
  # auto colors per type
  types <- sort(unique(item_types$type))
  n_types <- length(types)
  type_colors <- data.frame(
    type  = types,
    color = grDevices::hcl(h = seq(15, 375, length.out = n_types + 1)[1:n_types],
                           c = 80, l = 50),
    stringsAsFactors = FALSE
  )
  
  # grid size and call into C++
  n_plots   <- length(unique(wdf$rhs))
  grid_size <- ceiling(sqrt(n_plots))
  
  layout <- niarules::buildCoralPlots(
    wdf,
    grid_size     = grid_size,
    edge_gradient = c("#440154", "#21908C", "#FDE725"),
    edge_metric   = "confidence",
    item_types    = item_types,
    type_colors   = type_colors
  )
  
  list(
    nodes     = layout$nodes,
    edges     = layout$edges,
    edge_metric = layout$edge_metric,
    edge_range = layout$edge_range,
    grid_size = grid_size
  )
}