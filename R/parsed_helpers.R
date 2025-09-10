#' List available RHS composite labels
#' @param parsed A list as returned by `parse_rules()`, with components
#'   `items` and `rules`.
#' @param ignore_order Logical; if `TRUE` (default), items in an itemset are
#'   sorted alphabetically before composing labels.
#' @export
list_rhs_labels <- function(parsed, ignore_order = TRUE) {
  stopifnot(is.list(parsed), !is.null(parsed$items), !is.null(parsed$rules))
  ids_to_label <- function(ids) {
    labs <- parsed$items$label[match(ids, parsed$items$item_id)]
    if (ignore_order) labs <- sort(labs)
    paste(labs, collapse = " + ")
  }
  uniq <- unique(vapply(parsed$rules$rhs_item_ids, ids_to_label, ""))
  sort(uniq[nchar(uniq) > 0])
}

#' RHS label per rule (aligned vector)
#' @param parsed A list as returned by `parse_rules()`, with components
#'   `items` and `rules`.
#' @param ignore_order Logical; if `TRUE` (default), items in an itemset are
#'   sorted alphabetically before composing labels.
#' @export
rhs_label_per_rule <- function(parsed, ignore_order = TRUE) {
  ids_to_label <- function(ids) {
    labs <- parsed$items$label[match(ids, parsed$items$item_id)]
    if (ignore_order) labs <- sort(labs)
    paste(labs, collapse = " + ")
  }
  vapply(parsed$rules$rhs_item_ids, ids_to_label, "")
}

#' Filter parsed object to a single RHS and remap item ids to 0..K-1
#' @param parsed A list as returned by `parse_rules()`.
#' @param rhs_label Character; the composite RHS label to filter by, e.g.
#'   `"Milk + Bread"`. Must match the formatting used by
#'   `rhs_label_per_rule(ignore_order = TRUE/FALSE)`.
#' @param ignore_order Logical; treat RHS itemsets as unordered when matching
#'   `rhs_label` if `TRUE`.
#' @export
filter_parsed_by_rhs <- function(parsed, rhs_label, ignore_order = TRUE) {
  labs <- rhs_label_per_rule(parsed, ignore_order = ignore_order)
  keep <- labs == rhs_label
  parsed$rules <- parsed$rules[keep, , drop = FALSE]
  # collect used item_ids
  used <- sort(unique(c(unlist(parsed$rules$lhs_item_ids), unlist(parsed$rules$rhs_item_ids))))
  map <- setNames(seq_along(used) - 1L, used)
  relabel <- function(ids) unname(map[match(ids, as.integer(names(map)))])
  parsed$rules$lhs_item_ids <- lapply(parsed$rules$lhs_item_ids, relabel)
  parsed$rules$rhs_item_ids <- lapply(parsed$rules$rhs_item_ids, relabel)
  parsed$items <- parsed$items[parsed$items$item_id %in% used, , drop = FALSE]
  parsed$items$item_id <- relabel(parsed$items$item_id)
  parsed
}

#' Stable node color map (HCL)
#' @param parsed A list as returned by `parse_rules()`.
#' @param by Character; color mapping domain. One of `"type"` (feature) or
#'   `"item"` (item label).
#' @param hcl_c Numeric chroma value passed to `grDevices::hcl()`.
#' @param hcl_l Numeric luminance value passed to `grDevices::hcl()`.
#' @export
make_node_color_map <- function(parsed, by = c("type","item"), hcl_c = 65, hcl_l = 70) {
  by <- match.arg(by)
  key <- if (by == "type") parsed$items$feature else parsed$items$label
  uniq <- sort(unique(na.omit(key)))
  hues <- seq(0, 360, length.out = length(uniq) + 1)[-1]
  cols <- grDevices::hcl(h = hues, c = hcl_c, l = hcl_l)
  setNames(cols, uniq)
}
