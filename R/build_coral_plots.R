#' @title Build coral plot layout (nodes + edges) from a parsed rules object
#'
#' @description
#' Produces the node and edge layout consumed by `render_coral_rgl()`.
#' Given a parsed association-rules object (from `parse_rules()`), this function
#' groups rules by RHS itemset (one coral per unique RHS), arranges those corals
#' on a square grid, and emits geometry and metadata for drawing.
#'
#' **Input expectations (`parsed`)**
#' - `parsed$items`: `data.frame` with at least
#'   `item_id` (integer, **0-based**), `label` (character).
#' - `parsed$rules`: `data.frame` with at least
#'   `support`, `confidence`, `lift` (numeric) and
#'   `lhs_item_ids`, `rhs_item_ids` (list-columns of **0-based** integer vectors).
#'
#' @param parsed A list as returned by `parse_rules()`, containing components
#'   `items` and `rules` with the schema above.
#' @param lhs_sort_metric character; how to order items **within each LHS path**
#'   when building the layout. One of `"confidence"`, `"support"`, `"lift"`.
#'   Typically interpreted as **descending** by the chosen metric.
#'
#' @details
#' **Grid sizing.** The number of corals (`n_plots`) is computed as the number of
#' distinct, non-empty RHS itemsets across rules. An RHS itemset’s display label
#' is recomposed by joining the `items$label` values for its `rhs_item_ids`
#' (comma-separated). The grid is arranged as a near-square:
#' `grid_size = ceiling(sqrt(n_plots))`, with a minimum of 1.
#'
#' The heavy lifting (node positions, radii, edge routing) is delegated to the
#' C++ backend `build_layout_cpp()`, which receives the `parsed` object, the
#' computed `grid_size`, and the chosen `lhs_sort_metric`.
#'
#' **Output schema (for `render_coral_rgl()`).**
#' - `nodes` includes (at least): `x`, `z`, `x_offset`, `z_offset`, `radius`,
#'   `path` (character key), and optionally `item`, `feature`, `step`,
#'   `interval_label`, `interval_label_short`.
#' - `edges` includes (at least): `x`, `y`, `z`, `x_end`, `y_end`, `z_end`,
#'   `parent_path`, `child_path`, and the rule metrics `support`, `confidence`, `lift`.
#'   (Initial `y`/`y_end` are typically on the base plane; vertical styling can be
#'   added later by `render_coral_rgl()` via `y_scale`/jitter.)
#'
#' **Indexing note.** Item identifiers remain **0-based** as produced by
#' `parse_rules()` for cross-language stability.
#'
#' @return
#' A list with components:
#' - `nodes`: `data.frame` of node geometry and labels,
#' - `edges`: `data.frame` of edge geometry and attached metrics,
#' - `grid_size`: integer grid side length used to arrange corals.
#'
#' @export
build_coral_plots <- function(
    parsed,
    lhs_sort_metric = c("confidence","support","lift")
) {
  lhs_sort_metric <- match.arg(lhs_sort_metric)
  
  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  items <- parsed$items
  rules <- parsed$rules
  stopifnot(is.data.frame(items), is.data.frame(rules))
  
  # figure out grid size = sqrt(unique RHS-combos)
  # (recompose the RHS label per rule by joining the item labels)
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
    lhs_sort     = lhs_sort_metric
  )
  
  # pass through and attach computed grid_size for the renderer
  out$grid_size <- grid_size
  out
}