#' Build layout (nodes + edges) from a parsed rules object
#'
#' @param parsed result from parse_rules()
#' @param lhs_sort_metric how to sort items inside each LHS path
#'        one of "confidence", "support", "lift"
#' @param edge_metric which metric to map to edge width/color
#'        one of "confidence", "lift", "support"
#' @param edge_gradient character vector of hex colors (>=2) used for edges
#' @return list(nodes=data.frame, edges=data.frame, edge_metric, edge_range, grid_size)
#' @export
build_coral_layout <- function(
  parsed,
  lhs_sort_metric = c("confidence","support","lift"),
  edge_metric     = c("confidence","lift","support"),
  edge_gradient   = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B")
) {
  lhs_sort_metric <- match.arg(lhs_sort_metric)
  edge_metric     <- match.arg(edge_metric)

  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  items <- parsed$items
  rules <- parsed$rules
  stopifnot(is.data.frame(items), is.data.frame(rules))

  # figure out grid size = sqrt(unique RHS-combos)
  # We recompose the RHS label per rule by joining the item labels.
  lab_by_id <- setNames(items$label, items$item_id)
  rhs_labels <- vapply(seq_len(nrow(rules)), function(i) {
    ids <- unlist(rules$rhs_item_ids[[i]], use.names = FALSE)
    if (length(ids) == 0L) "" else paste(lab_by_id[as.character(ids)], collapse = ", ")
  }, character(1))
  n_plots   <- max(1L, length(unique(rhs_labels[nzchar(rhs_labels)])))
  grid_size <- ceiling(sqrt(n_plots))

  # call C++: consumes the parsed list, computes layout, returns nodes/edges + meta
  out <- build_layout_cpp(
    parsed       = parsed,
    grid_size    = grid_size,
    lhs_sort     = lhs_sort_metric,
    edge_metric  = edge_metric,
    edge_gradient= edge_gradient
  )

  # pass through and attach computed grid_size for the renderer
  out$grid_size <- grid_size
  out
}
