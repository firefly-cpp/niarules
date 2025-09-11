#' @title Build coral plot layout (nodes + edges) from a parsed rules object
#'
#' @description
#' Produces the node and edge layout consumed by `render_coral_rgl()` or `render_coral_rgl_experimental()`.
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
#' @param bin_breaks Optional named list of numeric break vectors used to bin numeric features
#'   (e.g., `list(Age = c(0, 18, 30, 50, Inf))`). If `NULL`, bins are inferred later
#'   (and may be provided to the renderer via `bin_legend`).
#' @param bin_digits Integer number of decimal places used when formatting numeric
#'   interval labels (e.g., `[0.405,0.485)`). Default is `3`.
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
#'   `path` (character key), `node_id`, `is_root`, `coral_id`, `interval_brackets`, `bin_index`
#'    and optionally `item`, `feature`, `step`,
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
#' - `bin_legend`: data.frame (or NULL) mapping feature -> bin index -> interval.
#'
#' @export
build_coral_plots <- function(
    parsed,
    lhs_sort_metric = c("confidence","support","lift"),
    bin_breaks = NULL,
    bin_digits = 3
) {
  lhs_sort_metric <- match.arg(lhs_sort_metric)
  
  stopifnot(is.list(parsed), all(c("items","rules") %in% names(parsed)))
  items <- parsed$items
  rules <- parsed$rules
  stopifnot(is.data.frame(items), is.data.frame(rules))
  
  # figure out grid size = sqrt(unique RHS-combos)
  # (recompose the RHS label per rule by joining the item labels)
  lab_by_id <- stats::setNames(items$label, items$item_id)
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
  
  nodes <- out$nodes
  if (!nrow(nodes)) {
    out$grid_size <- grid_size
    # also return empty scaffolding for new columns to keep callers happy
    nodes$node_id <- integer(0)
    nodes$is_root <- logical(0)
    nodes$coral_id <- character(0)
    nodes$interval_brackets <- character(0)
    nodes$bin_index <- integer(0)
    out$nodes <- nodes
    out$bin_legend <- NULL
    return(out)
  }
  
  .fmt_iv <- function(lo, hi, inc_lo, inc_hi, digits = 3) {
    if (!is.finite(lo) && !is.finite(hi)) return(NA_character_)
    lbr <- if (isTRUE(inc_lo)) "[" else "("
    rbr <- if (isTRUE(inc_hi)) "]" else ")"
    paste0(
      lbr,
      if (is.finite(lo)) formatC(lo, digits = digits, format = "f") else "-Inf",
      ",",
      if (is.finite(hi)) formatC(hi, digits = digits, format = "f") else "Inf",
      rbr
    )
  }
  
  # stable id
  nodes$node_id <- seq_len(nrow(nodes))
  
  # convenience flags/keys
  nodes$is_root <- if ("step" %in% names(nodes)) nodes$step == 0L else FALSE
  nodes$coral_id <- paste0(sprintf("%.6f", nodes$x_offset), "_", sprintf("%.6f", nodes$z_offset))
  
  # choose feature column name that exists
  feature_col <- if ("feature" %in% names(nodes)) "feature" else
    if ("base_feature_name" %in% names(nodes)) "base_feature_name" else NULL
  
  # bracket-only text for numeric items (e.g., "[0.405,0.485)")
  .is_num <- if ("kind" %in% names(nodes)) nodes$kind == "numeric" else rep(FALSE, nrow(nodes))
  nodes$interval_brackets <- NA_character_
  if (all(c("interval_low","interval_high","incl_low","incl_high") %in% names(nodes))) {
    nodes$interval_brackets[.is_num] <- mapply(
      .fmt_iv,
      nodes$interval_low[.is_num],
      nodes$interval_high[.is_num],
      nodes$incl_low[.is_num],
      nodes$incl_high[.is_num],
      MoreArgs = list(digits = bin_digits)
    )
  }
  
  .legend_from_breaks <- function(br, digits = 3) {
    if (length(br) < 2) return(NULL)
    data.frame(
      bin = seq_len(length(br) - 1L),
      interval = paste0(
        "[",
        formatC(br[-length(br)], digits = digits, format = "f"),
        ",",
        formatC(br[-1L],         digits = digits, format = "f"),
        ")"
      ),
      stringsAsFactors = FALSE
    )
  }
  
  bin_legend <- NULL
  nodes$bin_index <- NA_integer_
  
  if (is.list(bin_breaks) && length(bin_breaks) && !is.null(feature_col)) {
    # build legend
    bl <- lapply(intersect(names(bin_breaks), unique(nodes[[feature_col]][.is_num])), function(f) {
      br <- sort(unique(as.numeric(bin_breaks[[f]])))
      L <- .legend_from_breaks(br, digits = bin_digits)
      if (is.null(L)) return(NULL)
      L$feature <- f
      L
    })
    bin_legend <- if (length(Filter(Negate(is.null), bl))) do.call(rbind, bl) else NULL
    
    # map to bin_index by midpoint
    if (!is.null(bin_legend)) {
      mid <- (nodes$interval_low + nodes$interval_high) / 2
      for (f in unique(bin_legend$feature)) {
        br <- sort(unique(as.numeric(bin_breaks[[f]])))
        if (length(br) < 2) next
        idx <- .is_num & nodes[[feature_col]] == f & is.finite(mid)
        nodes$bin_index[idx] <- findInterval(mid[idx], br, left.open = FALSE, rightmost.closed = FALSE)
        nodes$bin_index[idx] <- ifelse(
          nodes$bin_index[idx] %in% seq_len(length(br) - 1L),
          nodes$bin_index[idx], NA_integer_
        )
      }
    }
  }
  
  # write back & return
  out$nodes <- nodes
  out$grid_size <- grid_size
  out$bin_legend <- bin_legend
  out
}