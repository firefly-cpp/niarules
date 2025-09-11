#' @title Apply styling to coral plots and render them with rgl
#'
#' @description
#' Renders a 3D "coral" plot produced by `build_coral_layout()`, with
#' edge width/color/alpha mapped from association rule metrics and node
#' colors derived from item/type groupings. The function draws a floor grid,
#' edges as 3D segments, nodes as spheres, and optional labels/legend.
#'
#' **Required columns**
#' - `edges`: `x`, `y`, `z`, `x_end`, `y_end`, `z_end`,
#'   `parent_path`, `child_path`, and metric columns `support`, `confidence`, `lift`.
#' - `nodes`: `x`, `z`, `x_offset`, `z_offset`, `radius`, `path`.
#'
#' **Optional columns**
#' - `nodes$item`, `nodes$feature` (for labels/legend & color-by),
#'   `nodes$step` (roots identified as `step == 0`),
#'   `nodes$interval_label`, `nodes$interval_label_short` (label text when requested).
#'
#' @param nodes data.frame; typically `build_coral_layout()$nodes`. Must contain
#'   `x`, `z`, `x_offset`, `z_offset`, `radius`, `path`. Optional: `item`,
#'   `feature`, `step`, `interval_label`, `interval_label_short`.
#' @param edges data.frame; typically `build_coral_layout()$edges`. Must contain
#'   `x`, `y`, `z`, `x_end`, `y_end`, `z_end`, `parent_path`, `child_path`,
#'   and metric columns `support`, `confidence`, `lift`.
#' @param grid_size integer; the layout grid size (usually `build_coral_layout()$grid_size`).
#' @param grid_color background grid color. Any R color spec. Default `"grey80"`.
#' @param legend logical; draw a node legend keyed by base feature (`nodes$feature`).
#'   Requires that `nodes$feature` and node colors are available. Default `FALSE`.
#' @param label_mode one of `"none"`, `"interval"`, `"item"`, `"interval_short"`.
#'   Controls label text: interval labels, item labels, or no labels.
#' @param label_cex numeric; label size passed to `rgl::text3d()`. Default `0.7`.
#' @param label_offset numeric; vertical offset (in **node radii**) applied
#'   to labels (positive values move labels downward from sphere tops). Default `1.5`.
#' @param max_labels integer; maximum number of **non-root** labels to keep (largest radii first).
#'   Root nodes are always kept. Default `100`.
#'
#' @param edge_width_metric character; which metric to map to edge **width**.
#'   One of `"confidence"`, `"lift"`, `"support"`. Default `"confidence"`.
#' @param edge_color_metric character; which metric to map to edge **color**.
#'   One of `"confidence"`, `"lift"`, `"support"`. Default `"confidence"`.
#' @param edge_alpha_metric character or `NULL`; which metric to map to edge **alpha**
#'   (transparency). One of `"support"`, `"lift"`, `"confidence"`, or `NULL` to use the
#'   constant `edge_alpha`. Default `NULL`.
#'
#' @param edge_width_range numeric length-2; min/max line width for edges after scaling.
#'   Default `c(1, 5)`.
#' @param edge_width_transform character; transformation for width scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param edge_gradient character vector (>= 2); color ramp for edges, passed to
#'   `grDevices::colorRamp()`. Default
#'   `c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B")`.
#' @param edge_color_transform character; transformation for color scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param edge_alpha numeric in `[0,1]`; constant alpha used **only when**
#'   `edge_alpha_metric` is `NULL`. Default `0.6`.
#' @param edge_alpha_range numeric length-2 in `[0,1]`; min/max alpha used **only when**
#'   `edge_alpha_metric` is not `NULL`. Default `c(0.25, 0.5)`.
#' @param edge_alpha_transform character; transformation for alpha scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param node_color_by one of `"type"`, `"item"`, `"none"`, `"edge_incoming"`, `"edge_outgoing_mean"`.
#'   Controls node coloring:
#'   - `"type"` colors by `nodes$feature` (recommended).
#'   - `"item"` colors by `nodes$item`.
#'   - `"none"` leaves default colors.
#'   - `"edge_incoming"` / `"edge_outgoing_mean"` are reserved for future use.
#'   **Note:** current implementation applies custom colors only for `"type"` and `"item"`.
#'   Default `"type"`.
#' @param node_gradient either the string `"match"` to reuse `edge_gradient` for nodes,
#'   or a character vector (>= 2) of colors to build the node palette. Default `"match"`.
#' @param node_gradient_map one of `"even"`, `"hash"`, `"frequency"`; how unique labels are
#'   placed along the gradient:
#'   - `"even"`: evenly spaced by sorted unique label order,
#'   - `"hash"`: stable per-label positions via a lightweight hash (good for reproducibility),
#'   - `"frequency"`: labels ordered by frequency (most frequent near one end).
#'   Default `"even"`.
#'
#' @param y_scale numeric scalar; vertical scale factor applied to each node’s normalized
#'   radial distance from its local center (`x_offset`,`z_offset`). `0` keeps the
#'   plot flat; try `0.5`–`0.8` for gentle relief. Default `0`.
#'
#' @param jitter_sd numeric; standard deviation of vertical jitter added to nodes,
#'   multiplied by the normalized radius so jitter fades toward the center. Default `0`.
#' @param jitter_mode one of `"deterministic"` or `"random"`. Deterministic jitter
#'   derives noise from `nodes$path` (requires that column); random jitter uses `rnorm()`.
#'   Default `"deterministic"`.
#' @param jitter_seed integer or `NULL`; RNG seed for reproducible **random** jitter.
#'   Ignored for `"deterministic"` mode. Default `NULL`.
#'
#' @param return_data logical; if `TRUE`, returns a list with augmented `nodes` and `edges`
#'   (including computed `color`, `width`, `y`, etc.) instead of just drawing. The plot is
#'   still created. Default `FALSE`.
#'
#' @details
#' Metric scaling uses the helper `.norm_metric()` which:
#' 1) rescales the chosen metric to `[0,1]` over finite values, and
#' 2) applies the selected transform:
#'    - `"linear"`: identity,
#'    - `"sqrt"`: emphasizes differences at the low end,
#'    - `"log"`: `log1p(9*t)/log(10)`, emphasizing very small values.
#'
#' Node elevation (`y`) is computed as `y_scale * r_norm` where `r_norm` is the node’s
#' radial distance from its center normalized to the max within that coral. Optional jitter
#' is added (fading to zero at the center). Root nodes (`step == 0`) that overlap are
#' vertically stacked with small stems for readability.
#'
#' @return
#' Invisibly returns `NULL` after drawing. If `return_data = TRUE`, returns (invisibly)
#' a list with components:
#' - `nodes`: input `nodes` with added columns `y`, `color` (and possibly stacked
#'   draw positions for roots),
#' - `edges`: input `edges` with added columns `width`, `color`, `t_color_norm`,
#'   `y`, `y_end`, and `width_binned`.
#'
#' @section Requirements:
#' Requires an interactive OpenGL device (`rgl`). On headless systems, consider
#' using an off-screen context or skipping examples.
#'
#' @importFrom rgl open3d par3d aspect3d lines3d segments3d spheres3d view3d text3d material3d bgplot3d
#' @importFrom dplyr distinct
#' @export
render_coral_rgl <- function(
    nodes, edges, grid_size,
    grid_color   = "grey80",
    legend       = FALSE,
    label_mode   = c("none", "interval", "item", "interval_short"),
    label_cex    = 0.7,
    label_offset = 1.5,
    max_labels   = 100,
    
    edge_width_metric   = c("confidence","lift","support"),
    edge_color_metric   = c("confidence","lift","support"),
    edge_alpha_metric   = NULL,  # e.g. "support" | "lift" | "confidence" | NULL
    
    # width controls
    edge_width_range    = c(1, 5),
    edge_width_transform= c("linear","sqrt","log"),
    
    # color controls
    edge_gradient       = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
    edge_color_transform= c("linear","sqrt","log"),
    
    # alpha controls
    edge_alpha          = 0.5,
    edge_alpha_range    = c(0.25, 0.5),
    edge_alpha_transform= c("linear","sqrt","log"),
    
    node_color_by     = c("type","item","none","edge_incoming","edge_outgoing_mean"),
    node_gradient     = "match",   # or a vector of hex colors for nodes
    node_gradient_map = c("even","hash","frequency"),
    
    y_scale = 0,        # << 0 keeps current "flat" look; try 0.5–0.8
    jitter_sd    = 0.0,
    jitter_mode  = c("deterministic","random"),
    jitter_seed  = NULL,
    
    return_data = FALSE
) {
  label_mode           <- match.arg(label_mode)
  node_color_by     <- match.arg(node_color_by)
  node_gradient_map <- match.arg(node_gradient_map)
  edge_width_metric    <- match.arg(edge_width_metric)
  edge_color_metric    <- match.arg(edge_color_metric)
  edge_width_transform <- match.arg(edge_width_transform)
  edge_color_transform <- match.arg(edge_color_transform)
  jitter_mode          <- match.arg(jitter_mode)
  if (!is.null(edge_alpha_metric)) edge_alpha_transform <- match.arg(edge_alpha_transform)
  
  #### helpers
  .norm_metric <- function(x, transform = c("linear","sqrt","log")) {
    x <- as.numeric(x)
    r <- range(x, finite = TRUE)
    if (!is.finite(r[1]) || !(r[2] > r[1])) {
      t <- rep(0.5, length(x))
    } else {
      t <- (x - r[1]) / (r[2] - r[1])
    }
    transform <- match.arg(transform)
    if (transform == "sqrt") t <- sqrt(pmax(0, t))
    if (transform == "log")  t <- log1p(pmax(0, t) * 9) / log(10)
    t[!is.finite(t)] <- 0.5
    t
  }
  
  # small deterministic noise in [-1,1] from a string (doesn't touch RNG)
  .hash_noise <- function(keys, salt = 0) {
    vapply(keys, function(k) {
      if (is.na(k) || !nzchar(k)) return(0)
      s <- sum(utf8ToInt(k)) + salt * 131071
      frac <- (sin(s * 12.9898 + 78.233) * 43758.5453) %% 1
      (frac - 0.5) * 2
    }, numeric(1))
  }
  
  #### edge styling (color, width, alpha)
  miss_cols <- setdiff(c(edge_width_metric, edge_color_metric, edge_alpha_metric), names(edges))
  if (length(miss_cols)) stop("edges missing required metric columns: ", paste(miss_cols, collapse = ", "))
  
  tw <- .norm_metric(edges[[edge_width_metric]], edge_width_transform)
  edges$width <- edge_width_range[1] + tw * (edge_width_range[2] - edge_width_range[1])
  
  tc <- .norm_metric(edges[[edge_color_metric]], edge_color_transform)
  if (length(edge_gradient) < 2L) edge_gradient <- c("#2166AC","#B2182B")
  cr  <- grDevices::colorRamp(edge_gradient)
  rgb <- cr(tc)
  
  if (is.null(edge_alpha_metric)) {
    a <- rep(edge_alpha, length(tc))
  } else {
    ta <- .norm_metric(edges[[edge_alpha_metric]], edge_alpha_transform)
    a  <- edge_alpha_range[1] + ta * (edge_alpha_range[2] - edge_alpha_range[1])
  }
  a <- pmin(pmax(a, 0), 1)
  edges$color <- grDevices::rgb(rgb[,1]/255, rgb[,2]/255, rgb[,3]/255, alpha = 0.1, maxColorValue = 1)
  edges$alpha <- a
  edges$t_color_norm <- tc
  
  #### node styling (color)
  # choose the key: type or item
  if (node_color_by %in% c("type","item")) {
    key <- if (node_color_by == "type") as.character(nodes$feature) else as.character(nodes$item)
    
    # pick the gradient for nodes: either reuse edge gradient ("match") or use user-supplied
    grad_nodes <- if (is.character(node_gradient) &&
                      length(node_gradient) == 1L &&
                      identical(node_gradient, "match")) {
      edge_gradient
    } else {
      node_gradient
    }
    if (length(grad_nodes) < 2L) grad_nodes <- c("#444444", "#BBBBBB")
    
    cr_nodes <- grDevices::colorRamp(grad_nodes)
    
    # unique labels
    uniq <- sort(unique(na.omit(key)))
    
    # map labels -> positions t in [0,1]
    if (length(uniq) == 1L) {
      tvals <- 0.5
    } else if (node_gradient_map == "hash") {
      # stable per-label t in [0,1] via a lightweight hash
      hash_to_unit <- function(s) {
        if (is.na(s) || !nzchar(s)) return(0.5)
        frac <- (sin(sum(utf8ToInt(s)) * 12.9898 + 78.233) * 43758.5453) %% 1
        as.numeric(frac)
      }
      tvals <- vapply(uniq, hash_to_unit, numeric(1))
    } else if (node_gradient_map == "frequency") {
      tab <- sort(table(key), decreasing = TRUE)
      uniq <- names(tab)
      tvals <- seq(0, 1, length.out = length(uniq))
    } else { # "even"
      tvals <- seq(0, 1, length.out = length(uniq))
    }
    
    # build palette from gradient positions
    cols_mat <- cr_nodes(pmin(pmax(tvals, 0), 1))
    lut <- setNames(grDevices::rgb(cols_mat[,1], cols_mat[,2], cols_mat[,3], maxColorValue = 255), uniq)
    
    # assign colors to nodes
    nodes$color <- lut[key]
    nodes$color[is.na(nodes$color) | !nzchar(nodes$color)] <- "black"
  }
  
  #### y from radius (styling) + optional jitter
  # base radial distance and per-plot normalization (so max radius -> 1)
  r <- sqrt((nodes$x - nodes$x_offset)^2 + (nodes$z - nodes$z_offset)^2)
  key_center <- paste0(sprintf("%.6f", nodes$x_offset), "_", sprintf("%.6f", nodes$z_offset))
  r_max <- ave(r, key_center, FUN = function(v) if (length(v) && max(v) > 0) max(v) else 1)
  r_norm <- ifelse(r_max > 0, r / r_max, 0)
  
  # base y
  nodes$y <- y_scale * r_norm
  
  # jitter that fades to 0 at the center (multiplied by r_norm)
  if (jitter_sd > 0) {
    if (jitter_mode == "random") {
      if (!is.null(jitter_seed)) {
        old_seed <- .Random.seed
        on.exit({ if (exists("old_seed")) assign(".Random.seed", old_seed, envir = .GlobalEnv) }, add = TRUE)
        set.seed(jitter_seed)
      }
      noise <- stats::rnorm(nrow(nodes))
    } else {
      if (!("path" %in% names(nodes))) stop("nodes must have a 'path' column for deterministic jitter.")
      noise <- .hash_noise(as.character(nodes$path), salt = 1L)
    }
    nodes$y <- nodes$y + (jitter_sd * r_norm) * noise
  }
  
  #### propagate node y to edges using the path keys
  if (!all(c("parent_path","child_path") %in% names(edges))) {
    stop("edges must have 'parent_path' and 'child_path' columns.")
  }
  if (!("path" %in% names(nodes))) {
    stop("nodes must have a 'path' column.")
  }
  idx_parent <- match(edges$parent_path, nodes$path)
  idx_child  <- match(edges$child_path,  nodes$path)
  edges$y     <- nodes$y[idx_parent]
  edges$y_end <- nodes$y[idx_child]
  
  #### draw
  rgl::open3d()
  rgl::par3d(windowRect = c(50, 50, 1000, 1000))
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  rgl::view3d(theta = 0, phi = phi_deg, fov = 60)
  rgl::aspect3d(1, 1, 1)
  rgl::par3d(skipRedraw = TRUE)
  
  xlim <- c(0, grid_size); zlim <- c(0, grid_size)
  xs   <- seq(xlim[1], xlim[2], by = 1)
  zs   <- seq(zlim[1], zlim[2], by = 1)
  for (z in zs) rgl::lines3d(x = xlim, y = 0, z = c(z, z), color = grid_color)
  for (x in xs) rgl::lines3d(x = x, y = c(0, 0), z = zlim, color = grid_color)
  
  node_cols <- if ("color" %in% names(nodes)) {
    col <- as.character(nodes$color); col[is.na(col) | !nzchar(col)] <- "black"; col
  } else "black"
  
  is_root <- if ("step" %in% names(nodes)) nodes$step == 0L else rep(FALSE, nrow(nodes))
  y_draw  <- nodes$y
  idx     <- which(is_root)
  
  # optional stacking for coincident roots
  if (length(idx)) {
    stack_gap <- 0.04
    key <- paste0(sprintf("%.6f", nodes$x_offset[idx]), "_", sprintf("%.6f", nodes$z_offset[idx]))
    groups <- split(idx, key)
    for (g in groups) {
      if (length(g) > 1L) {
        ranks <- seq_len(length(g)) - (length(g) + 1) / 2
        y_draw[g] <- y_draw[g] + ranks * stack_gap
      }
    }
  }
  
  stems <- idx[abs(y_draw[idx] - nodes$y[idx]) > 1e-9]
  if (length(stems)) {
    segs <- as.numeric(t(cbind(
      nodes$x[stems], nodes$y[stems], nodes$z[stems],
      nodes$x[stems], y_draw[stems],  nodes$z[stems]
    )))
    rgl::segments3d(segs, color = "grey50", alpha = 0.5, depth_mask = FALSE, lwd = 2)
  }
  
  if (nrow(edges)) {
    edges$width_binned <- round(edges$width, 2)
    edges$alpha_binned <- round(edges$alpha, 3)
    styles <- unique(edges[c("width_binned","alpha_binned","color")])
    for (i in seq_len(nrow(styles))) {
      st <- styles[i, ]
      idx <- edges$width_binned == st$width_binned & edges$alpha_binned == st$alpha_binned & edges$color == st$color
      sub <- edges[idx, , drop = FALSE]
      coords <- as.numeric(t(cbind(sub$x, sub$y, sub$z, sub$x_end, sub$y_end, sub$z_end)))
      rgl::segments3d(coords, color = st$color, alpha = st$alpha_binned,
                      lwd = st$width_binned, depth_mask = FALSE)
    }
  }
  
  rgl::spheres3d(nodes$x, y_draw, nodes$z, radius = nodes$radius, color = node_cols)
  
  if (label_mode != "none") {
    txt <- if (label_mode == "interval" && "interval_label" %in% names(nodes)) {
      nodes$interval_label
    } else if (label_mode == "interval_short" && "interval_label_short" %in% names(nodes)) {
      nodes$interval_label_short
    } else {
      nodes$item
    }
    ord <- order(nodes$radius, decreasing = TRUE)
    keep_main <- head(ord, max_labels)
    keep <- sort(unique(c(keep_main, idx)))
    missing_root_txt <- (is.na(txt) | !nzchar(txt)) & is_root
    txt[missing_root_txt] <- nodes$item[missing_root_txt]
    
    rgl::material3d(depth_test = "always")
    rgl::text3d(
      x = nodes$x[keep],
      y = y_draw[keep] - nodes$radius[keep] * label_offset,
      z = nodes$z[keep],
      texts     = txt[keep],
      cex       = label_cex,
      color     = node_cols[keep],
      fixedSize = TRUE,
      adj       = c(0.5, 1)
    )
    rgl::material3d(depth_test = "less")
  }
  
  rgl::par3d(skipRedraw = FALSE)
  
  if (legend && "color" %in% names(nodes)) {
    if (!"feature" %in% names(nodes)) stop("nodes$feature missing; legend needs base feature names.")
    legend_df <- dplyr::distinct(nodes, label = .data$feature, color = .data$color)
    if (nrow(legend_df) > 0) {
      rgl::bgplot3d({
        op <- par(mar = c(0,0,0,0))
        plot.new()
        legend("topright", legend = legend_df$label, fill = legend_df$color,
               border = NA, bty = "n", cex = 0.8)
        par(op)
      })
    }
  }
  
  if (return_data) return(invisible(list(nodes = nodes, edges = edges)))
  invisible(NULL)
}


#' Coral 3D renderer (experimental)
#' @description
#' `r lifecycle::badge('experimental')`
#' API and argument names may change before 0.4.0.
#' @export
#' 
#' @description
#' Renders a 3D "coral" plot produced by `build_coral_layout()`, with
#' edge width/color/alpha mapped from association rule metrics and node
#' colors derived from item/type groupings. The function draws a floor grid,
#' edges as 3D segments, nodes as spheres, and optional labels/legend.
#'
#' **Required columns**
#' - `edges`: `parent_path`, `child_path`, and metric columns `support`, `confidence`, `lift`.
#'   (Endpoint coordinates `x`, `y`, `z`, `x_end`, `y_end`, `z_end` are recomputed by this function.)
#' - `nodes`: `x`, `z`, `x_offset`, `z_offset`, `radius`, `path`.
#'
#' **Optional columns**
#' - `nodes$item`, `nodes$feature` (for labels/legend & color-by),
#'   `nodes$step` (roots identified as `step == 0`),
#'   `nodes$interval_label`, `nodes$interval_label_short` (label text when requested).
#'
#' @param nodes data.frame; typically `build_coral_layout()$nodes`. Must contain
#'   `x`, `z`, `x_offset`, `z_offset`, `radius`, `path`. Optional: `item`,
#'   `feature`, `step`, `interval_label`, `interval_label_short`.
#' @param edges data.frame; typically `build_coral_layout()$edges`. Must contain
#'   `parent_path`, `child_path`, and metric columns `support`, `confidence`, `lift`.
#'   Endpoint columns are ignored if present and will be recomputed.
#' @param grid_size integer; the layout grid size (usually `build_coral_layout()$grid_size`).
#' @param grid_outline logical; if `TRUE` draws the reference grid/guide (using `grid_color`
#'   or the theme's grid color). Defaults to `FALSE` for clean screenshots.
#' @param grid_color color for the grid (if `grid_outline = TRUE`). If missing, the active
#'   theme's grid color is used. Default `"grey92"`.
#' @param legend logical; draw a node legend keyed by base feature (`nodes$feature`).
#'   Requires that `nodes$feature` and node colors are available. Default `FALSE`.
#' @param legend_style Character; how to compose the legend. One of
#'   `"auto"`, `"feature"`, `"grouped"`, `"feature_bins"`. `"auto"` picks
#'   `"feature_bins"` when binning info is available, otherwise `"feature"`.
#' @param legend_cex Numeric scaling factor for all legend text.
#' @param legend_pos Character; legend position. One of
#'   `"inside_right"`, `"topright"`, `"topleft"`, `"custom"`.
#'   `"custom"` uses `legend_xy`.
#' @param legend_items_per_feature Integer; maximum number of items to list per
#'   feature before eliding with "...".
#' @param legend_features_max Integer; maximum number of distinct features shown
#'   in the legend.
#' @param legend_xy Numeric length-2; normalized device coordinates `(x, y)` used
#'   when `legend_pos = "custom"`.
#' @param legend_panel_width Numeric (0-1); fraction of the viewport width
#'   reserved for the legend panel when drawing inside the 3D device.
#' @param legend_panel_margin Numeric (0-1); margin around the legend panel
#'   within the reserved area.
#' @param legend_reserve Optional numeric (0-1); if provided, overrides
#'   `legend_panel_width` as the fraction of viewport width to carve out for the
#'   legend.
#' @param legend_title_cex Optional numeric; cex override for the legend title.
#'   If `NULL`, a sensible default is used.
#' @param legend_row_cex Optional numeric; cex override for item rows in the
#'   legend. If `NULL`, a sensible default is used.
#' @param legend_col_gap Numeric; horizontal gap between legend columns (in
#'   normalized device coordinates).
#' @param label_mode one of `"none"`, `"interval"`, `"item"`, `"interval_short"`.
#'   Controls label text: interval labels, item labels, or no labels.
#' @param label_cex numeric; label size passed to `rgl::text3d()`. Default `0.7`.
#' @param label_offset numeric; vertical offset (in **node radii**) applied
#'   to labels (positive values move labels downward from sphere tops). Default `1.5`.
#' @param max_labels integer; maximum number of non-root labels (largest radii first).
#'   Root nodes are always kept. If `<= 0`, only root (RHS) labels are drawn.
#' @param label_color `NULL` to color labels like their nodes, or a single color / vector to override.
#' @param label_non_numeric Character; how to label non-numeric feature values.
#'   One of `"none"`, `"category"`, `"item"`. `"none"` suppresses labels for
#'   non-numeric nodes.
#' @param theme character; one of `"default"`, `"studio"`, `"flat"`, `"dark"`, `"none"`.
#'   Selects a preset for lights, materials, and background:
#'   - **default**: balanced lighting with subtle specular highlights.
#'   - **studio**: brighter, glossy look for screenshots.
#'   - **flat**: low-specular, diagram-style shading.
#'   - **dark**: dark background with rim lighting.
#'   - **none**: no lights configured (use existing rgl state/ambient).
#' @param theme_overrides optional named list to partially override the selected theme.
#'   Supported keys: `background` (color), `grid_color` (color),
#'   `materials` (list with sublists `nodes`, `edges`, `labels` - each passed to
#'   [rgl::material3d()]), and `lights` (list of argument lists for [rgl::light3d()]).
#'   Example: `list(lights = list(list(theta = 60, phi = 30)))`.
#' @param apply_theme logical; if `TRUE` (default) the theme is applied at the start of rendering
#'   (background, lights, global/material defaults). Set to `FALSE` to keep the current rgl device
#'   state (useful when you configure lights/materials once for batch rendering).
#' @param bin_legend Optional `data.frame` with columns `feature`, `bin`,
#'   `interval` (character), typically passed from `build_coral_plots()` to
#'   drive a `"feature_bins"` legend.
#' @param bin_breaks Optional named list `list(Feature = numeric breaks)` used
#'   to compute per-feature bins when `bin_legend` is not provided.
#' @param bin_infer Logical; if `TRUE`, infer binning from `nodes` when neither
#'   `bin_legend` nor `bin_breaks` is provided.
#' @param bin_label_fmt how numbers are printed, One of `"index`" or `"roman`". Default `"index`".
#' @param edge_width_domain,edge_color_domain,edge_alpha_domain
#'   optional numeric length-2 vectors giving the global domain (min, max)
#'   to use when scaling the respective metric. If `NULL` (default),
#'   the domain is computed from the provided `edges`. Use these to enforce
#'   consistent scaling across multiple plots (e.g., faceting).
#'
#' @param edge_width_metric character; which metric to map to edge **width**.
#'   One of `"confidence"`, `"lift"`, `"support"`. Default `"confidence"`.
#' @param edge_color_metric character; which metric to map to edge **color**.
#'   One of `"confidence"`, `"lift"`, `"support"`. Default `"confidence"`.
#' @param edge_alpha_metric character or `NULL`; which metric to map to edge **alpha**
#'   (transparency). One of `"support"`, `"lift"`, `"confidence"`, or `NULL` to use the
#'   constant `edge_alpha`. Default `NULL`.
#'
#' @param edge_width_range numeric length-2; min/max line width for edges after scaling.
#'   Default `c(1, 5)`.
#' @param edge_width_transform character; transformation for width scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param edge_gradient character vector (>= 2); color ramp for edges, passed to
#'   `grDevices::colorRamp()`. Default
#'   `c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B")`.
#' @param edge_color_transform character; transformation for color scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param edge_alpha numeric in `[0,1]`; constant alpha used **only when**
#'   `edge_alpha_metric` is `NULL`. Default `0.5`.
#' @param edge_alpha_range numeric length-2 in `[0,1]`; min/max alpha used **only when**
#'   `edge_alpha_metric` is not `NULL`. Default `c(0.25, 0.5)`.
#' @param edge_alpha_transform character; transformation for alpha scaling from normalized
#'   metric in `[0,1]`. One of `"linear"`, `"sqrt"`, `"log"`. Default `"linear"`.
#'
#' @param node_color_by one of `"type"`, `"item"`, `"none"`, `"edge_incoming"`, `"edge_outgoing_mean"`.
#'   Controls node coloring:
#'   - `"type"` colors by `nodes$feature`.
#'   - `"item"` colors by `nodes$item`.
#'   - `"none"` leaves existing/implicit colors.
#'   - `"edge_incoming"` uses the mean of incoming edges' normalized color metric (`t_color_norm`).
#'   - `"edge_outgoing_mean"` uses the mean of outgoing edges' normalized color metric.
#'   Default `"type"`.
#' @param node_gradient either the string `"match"` to reuse `edge_gradient` for nodes,
#'   or a character vector (>= 2) of colors to build the node palette. Default `"match"`.
#' @param node_gradient_map one of `"even"`, `"hash"`, `"frequency"`; how unique labels are
#'   placed along the gradient:
#'   - `"even"`: evenly spaced by sorted unique label order,
#'   - `"hash"`: stable per-label positions via a lightweight hash (reproducible),
#'   - `"frequency"`: labels ordered by frequency (most frequent near one end).
#'   Default `"even"`.
#' @param node_scale Numeric; global multiplier applied to node radii (size).
#' @param radial_expand numeric; global expand/contract factor applied radially
#'   from each plot center. `1` = no change; `>1` pushes nodes outward. Default `1`.
#' @param radial_gamma numeric; curvature of the radial remap. `1` = linear,
#'   `>1` separates outer rings (more spacing near the edge), `<1` compresses them.
#'   Default `1`.
#'
#' @param y_scale numeric scalar; vertical scale factor applied to each node's normalized
#'   radial distance from its local center (`x_offset`,`z_offset`). `0` keeps the
#'   plot flat; try `0.5`-`0.8` for gentle relief. Default `0`.
#'
#' @param jitter_sd numeric; standard deviation of vertical jitter added to nodes,
#'   multiplied by the normalized radius so jitter fades toward the center. Default `0`.
#' @param jitter_mode one of `"deterministic"` or `"random"`. Deterministic jitter
#'   derives noise from `nodes$path` (requires that column); random jitter uses `rnorm()`.
#'   Default `"deterministic"`.
#' @param jitter_seed integer or `NULL`; RNG seed for reproducible **random** jitter.
#'   Ignored for `"deterministic"` mode. Default `NULL`.
#' @param keep_camera Logical; if `TRUE`, keep the current rgl camera settings
#'   (angle, FOV, zoom). If `FALSE`, apply the view parameters below.
#' @param view_theta Numeric; azimuth angle passed to `rgl::view3d()`.
#' @param view_phi Optional numeric; elevation angle. If `NULL`, a sensible
#'   default based on grid size is used.
#' @param view_fov Numeric; field of view in degrees (passed to `rgl::view3d()`).
#' @param view_zoom Optional numeric; zoom factor passed to `rgl::par3d(zoom=)`.
#' @param view_userMatrix Optional 4x4 matrix passed to `rgl::par3d(userMatrix=)`
#'   to fully specify the camera transform (overrides theta/phi when provided).
#' @param return_data logical; if `TRUE`, returns a list with augmented `nodes` and `edges`
#'   (including computed `color`, `width`, `y`, etc.) instead of just drawing. The plot is
#'   still created. Default `FALSE`.
#'
#' @details
#' Metric scaling uses a helper that:
#' 1) rescales the chosen metric to `[0,1]` over finite values, and
#' 2) applies the selected transform:
#'    - `"linear"`: identity,
#'    - `"sqrt"`: emphasizes differences at the low end,
#'    - `"log"`: `log1p(9*t)/log(10)`, emphasizing very small values.
#'
#' Node elevation (`y`) is computed as `y_scale * r_norm` where `r_norm` is the node's
#' radial distance from its center normalized to the max within that coral. Optional jitter
#' is added (fading to zero at the center). Root nodes (`step == 0`) that overlap are
#' vertically stacked (with small stems drawn). Labels are rendered on top of geometry.
#'
#' @return
#' Invisibly returns `NULL` after drawing. If `return_data = TRUE`, returns (invisibly)
#' a list with components:
#' - `nodes`: input `nodes` with added columns like `y` (base elevation) and `color`.
#' - `edges`: input `edges` with added columns `width`, `color`, `alpha`, `t_color_norm`,
#'   `y`, `y_end`, `width_binned`, `alpha_binned`.
#'
#' @section Requirements:
#' Requires an interactive OpenGL device (`rgl`). On headless systems, consider
#' using an off-screen context or skipping examples.
#'
#' @seealso [rgl::material3d()], [rgl::light3d()]
#' @importFrom rgl open3d par3d aspect3d lines3d segments3d spheres3d view3d text3d material3d bgplot3d
#' @importFrom dplyr distinct
#' @export
render_coral_rgl_experimental <- function(
    nodes, edges,
    
    grid_size,
    grid_outline = FALSE,
    grid_color   = "grey92",
    
    legend       = FALSE,
    legend_style   = c("auto","feature","grouped", "feature_bins"),
    legend_cex     = 1.0,
    legend_pos     = c("inside_right","topright","topleft","custom"),
    legend_items_per_feature = 6L,
    legend_features_max      = 10L,
    legend_xy      = c(0.92, 0.96),
    legend_panel_width  = 0.28,
    legend_panel_margin = 0.02,
    legend_reserve = NULL,
    legend_title_cex = NULL,
    legend_row_cex   = NULL,
    legend_col_gap = 0.004,
    
    label_mode   = c("none", "interval", "item", "interval_short", "bin"),
    label_cex    = 0.7,
    label_offset = 1.5,
    label_color  = NULL,
    label_non_numeric = c("none","category","item"),
    max_labels   = 0,
    
    bin_legend = NULL,     # data.frame(feature, bin, interval)  (optional)
    bin_breaks = NULL,     # named list(Feature = numeric breaks) (optional)
    bin_infer  = TRUE,     # infer from nodes if nothing else is given
    bin_label_fmt = c("index","roman"),  # how to print bin numbers
    
    theme = c("default","studio","flat","dark","none"),
    theme_overrides = NULL,
    apply_theme  = TRUE,
    
    edge_width_domain = NULL,
    edge_color_domain = NULL,
    edge_alpha_domain = NULL,
    
    edge_width_metric   = c("confidence","lift","support"),
    edge_color_metric   = c("confidence","lift","support"),
    edge_alpha_metric   = NULL,  # e.g. "support" | "lift" | "confidence" | NULL
    
    edge_width_range    = c(1, 5),
    edge_width_transform= c("linear","sqrt","log"),
    
    edge_gradient       = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
    edge_color_transform= c("linear","sqrt","log"),
    
    edge_alpha          = 0.5,
    edge_alpha_range    = c(0.25, 0.5),
    edge_alpha_transform= c("linear","sqrt","log"),
    
    node_color_by     = c("type","item","none","edge_incoming","edge_outgoing_mean"),
    node_gradient     = "match",   # or a vector of hex colors for nodes
    node_gradient_map = c("even","hash","frequency"),
    node_scale   = 1.0, 
    
    radial_expand = 1.0,
    radial_gamma  = 1.0,
    
    y_scale = 0,
    jitter_sd    = 0.0,
    jitter_mode  = c("deterministic","random"),
    jitter_seed  = NULL,
    
    keep_camera = FALSE,
    view_theta  = 0,
    view_phi    = NULL,
    view_fov    = 60,
    view_zoom   = NULL,
    view_userMatrix = NULL,
    
    return_data = FALSE
) {
  
  # ensure a device exists so apply_theme doesn't create one
  if (rgl::rgl.cur() == 0) rgl::open3d()
  
  theme <- match.arg(theme)
  th <- coral_get_theme(theme, theme_overrides)
  if (missing(grid_color) && !is.null(th$grid_color)) grid_color <- th$grid_color
  if (isTRUE(apply_theme)) coral_apply_theme(th)
  
  # Use the formals directly (you declared them in the signature)
  legend_style <- match.arg(legend_style)
  legend_pos   <- match.arg(legend_pos)
  
  # Coerce / validate the numeric ones
  legend_cex <- as.numeric(legend_cex)
  legend_items_per_feature <- as.integer(legend_items_per_feature)
  legend_features_max      <- as.integer(legend_features_max)
  legend_xy <- as.numeric(legend_xy)
  if (length(legend_xy) != 2L) stop("legend_xy must be a length-2 numeric vector.")
  
  legend_style     <- match.arg(legend_style)
  label_mode       <- match.arg(label_mode)
  label_non_numeric<- match.arg(label_non_numeric)
  bin_label_fmt    <- match.arg(bin_label_fmt)
  
  label_mode           <- match.arg(label_mode)
  node_color_by     <- match.arg(node_color_by)
  node_gradient_map <- match.arg(node_gradient_map)
  edge_width_metric    <- match.arg(edge_width_metric)
  edge_color_metric    <- match.arg(edge_color_metric)
  edge_width_transform <- match.arg(edge_width_transform)
  edge_color_transform <- match.arg(edge_color_transform)
  jitter_mode          <- match.arg(jitter_mode)
  if (!is.null(edge_alpha_metric)) edge_alpha_transform <- match.arg(edge_alpha_transform)
  
  #### helpers
  .norm_metric <- function(x, transform = c("linear","sqrt","log"), domain = NULL) {
    x <- as.numeric(x)
    r <- if (is.null(domain)) range(x, finite = TRUE) else as.numeric(domain)
    if (!is.finite(r[1]) || !(r[2] > r[1])) {
      t <- rep(0.5, length(x))
    } else {
      t <- (x - r[1]) / (r[2] - r[1])
    }
    transform <- match.arg(transform)
    if (transform == "sqrt") t <- sqrt(pmax(0, t))
    if (transform == "log")  t <- log1p(pmax(0, t) * 9) / log(10)
    t[!is.finite(t)] <- 0.5
    t
  }
  
  .hash_noise <- function(keys, salt = 0) {
    vapply(keys, function(k) {
      if (is.na(k) || !nzchar(k)) return(0)
      s <- sum(utf8ToInt(k)) + salt * 131071
      frac <- (sin(s * 12.9898 + 78.233) * 43758.5453) %% 1
      (frac - 0.5) * 2
    }, numeric(1))
  }
  
  #### edge styling (color, width, alpha)
  miss_cols <- setdiff(c(edge_width_metric, edge_color_metric, edge_alpha_metric), names(edges))
  if (length(miss_cols)) stop("edges missing required metric columns: ", paste(miss_cols, collapse = ", "))
  
  tw <- .norm_metric(edges[[edge_width_metric]], edge_width_transform,  edge_width_domain)
  edges$width <- edge_width_range[1] + tw * (edge_width_range[2] - edge_width_range[1])
  
  tc <- .norm_metric(edges[[edge_color_metric]], edge_color_transform,  edge_color_domain)
  if (length(edge_gradient) < 2L) edge_gradient <- c("#2166AC","#B2182B")
  cr  <- grDevices::colorRamp(edge_gradient)
  rgb <- cr(tc)
  
  if (is.null(edge_alpha_metric)) {
    a <- rep(edge_alpha, length(tc))
  } else {
    ta <- .norm_metric(edges[[edge_alpha_metric]], edge_alpha_transform, edge_alpha_domain)
    a  <- edge_alpha_range[1] + ta * (edge_alpha_range[2] - edge_alpha_range[1])
  }
  a <- pmin(pmax(a, 0), 1)
  edges$color <- grDevices::rgb(rgb[,1]/255, rgb[,2]/255, rgb[,3]/255, alpha = a, maxColorValue = 1)
  edges$alpha <- a
  edges$t_color_norm <- tc
  
  #### node styling (color)
  if (node_color_by %in% c("type","item")) {
    key <- if (node_color_by == "type") as.character(nodes$feature) else as.character(nodes$item)
    
    # pick the gradient for nodes
    grad_nodes <- if (is.character(node_gradient) &&
                      length(node_gradient) == 1L &&
                      identical(node_gradient, "match")) {
      edge_gradient
    } else {
      node_gradient
    }
    if (length(grad_nodes) < 2L) grad_nodes <- c("#444444", "#BBBBBB")
    
    cr_nodes <- grDevices::colorRamp(grad_nodes)
    
    # unique labels
    uniq <- sort(unique(na.omit(key)))
    
    # map labels -> positions t in [0,1]
    if (length(uniq) == 1L) {
      tvals <- 0.5
    } else if (node_gradient_map == "hash") {
      # stable per-label t in [0,1] via a lightweight hash
      hash_to_unit <- function(s) {
        if (is.na(s) || !nzchar(s)) return(0.5)
        frac <- (sin(sum(utf8ToInt(s)) * 12.9898 + 78.233) * 43758.5453) %% 1
        as.numeric(frac)
      }
      tvals <- vapply(uniq, hash_to_unit, numeric(1))
    } else if (node_gradient_map == "frequency") {
      tab <- sort(table(key), decreasing = TRUE)
      uniq <- names(tab)
      tvals <- seq(0, 1, length.out = length(uniq))
    } else {
      tvals <- seq(0, 1, length.out = length(uniq))
    }
    
    # build palette from gradient positions
    cols_mat <- cr_nodes(pmin(pmax(tvals, 0), 1))
    lut <- setNames(grDevices::rgb(cols_mat[,1], cols_mat[,2], cols_mat[,3], maxColorValue = 255), uniq)
    
    # assign colors to nodes
    nodes$color <- lut[key]
    nodes$color[is.na(nodes$color) | !nzchar(nodes$color)] <- "black"
  }
  else if (node_color_by %in% c("edge_incoming","edge_outgoing_mean")) {
    # need edges$t_color_norm in [0,1]
    nodes$path <- as.character(nodes$path)
    edges$parent_path <- as.character(edges$parent_path)
    edges$child_path  <- as.character(edges$child_path)
    
    if (node_color_by == "edge_incoming") {
      t_by_node <- tapply(edges$t_color_norm, edges$child_path, mean, na.rm = TRUE)
    } else {
      t_by_node <- tapply(edges$t_color_norm, edges$parent_path, mean, na.rm = TRUE)
    }
    tvals <- t_by_node[nodes$path]
    tvals[!is.finite(tvals)] <- 0.5
    
    grad_nodes <- if (is.character(node_gradient) && length(node_gradient) == 1L && identical(node_gradient, "match"))
      edge_gradient else node_gradient
    if (length(grad_nodes) < 2L) grad_nodes <- c("#444444", "#BBBBBB")
    cr_nodes <- grDevices::colorRamp(grad_nodes)
    cols_mat <- cr_nodes(pmin(pmax(tvals, 0), 1))
    nodes$color <- grDevices::rgb(cols_mat[,1], cols_mat[,2], cols_mat[,3], maxColorValue = 255)
  }
  
  # y from radius (styling) + optional jitter
  # base radial distance and per-plot normalization (so max radius -> 1)
  r <- sqrt((nodes$x - nodes$x_offset)^2 + (nodes$z - nodes$z_offset)^2)
  key_center <- paste0(sprintf("%.6f", nodes$x_offset), "_", sprintf("%.6f", nodes$z_offset))
  r_max <- ave(r, key_center, FUN = function(v) if (length(v) && max(v) > 0) max(v) else 1)
  r_norm <- ifelse(r_max > 0, r / r_max, 0)
  
  # base y
  nodes$y <- y_scale * r_norm
  
  # jitter that fades to 0 at the center (multiplied by r_norm)
  if (jitter_sd > 0) {
    if (jitter_mode == "random") {
      if (!is.null(jitter_seed)) {
        old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
        on.exit({ if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv) }, add = TRUE)
        set.seed(jitter_seed)
      }
      noise <- stats::rnorm(nrow(nodes))
    } else {
      if (!("path" %in% names(nodes))) stop("nodes must have a 'path' column for deterministic jitter.")
      noise <- .hash_noise(as.character(nodes$path), salt = 1L)
    }
    nodes$y <- nodes$y + (jitter_sd * r_norm) * noise
  }
  
  #### draw
  if (rgl::rgl.cur() == 0) rgl::open3d()
  rgl::par3d(windowRect = c(0, 0, 1200, 800))
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  if (!isTRUE(keep_camera)) {
    rgl::view3d(
      theta = view_theta,
      phi   = if (is.null(view_phi)) phi_deg else view_phi,
      fov   = view_fov
    )
    if (!is.null(view_zoom))       rgl::par3d(zoom = view_zoom)
    if (!is.null(view_userMatrix)) rgl::par3d(userMatrix = view_userMatrix)
  }
  rgl::aspect3d(1, 1, 1)
  rgl::par3d(skipRedraw = TRUE)

  restore_vp <- NULL
  if (isTRUE(legend) && legend_style %in% c("feature","grouped","feature_bins")) {
    vp <- rgl::par3d("viewport")                 # c(x, y, width, height) in pixels
    reserve_frac <- (if (is.null(legend_reserve)) legend_panel_width else legend_reserve) +
      legend_panel_margin
    new_w <- max(1L, floor(vp[3] * (1 - reserve_frac)))  # width for the 3D scene
    if (new_w < vp[3]) {
      restore_vp <- vp
      rgl::par3d(viewport = c(vp[1], vp[2], new_w, vp[4]))
      on.exit(try(rgl::par3d(viewport = restore_vp), silent = TRUE), add = TRUE)
    }
  }
  
  if (grid_outline) {
    rgl::material3d(lit = FALSE)
    xlim <- c(0, grid_size); zlim <- c(0, grid_size)
    xs   <- seq(xlim[1], xlim[2], by = 1)
    zs   <- seq(zlim[1], zlim[2], by = 1)
    for (z in zs) rgl::lines3d(x = xlim, y = 0, z = c(z, z), color = grid_color)
    for (x in xs) rgl::lines3d(x = x, y = c(0, 0), z = zlim, color = grid_color)
  }
  
  node_cols <- if ("color" %in% names(nodes)) {
    col <- as.character(nodes$color); col[is.na(col) | !nzchar(col)] <- "black"; col
  } else "black"
  
  is_root <- if ("step" %in% names(nodes)) nodes$step == 0L else rep(FALSE, nrow(nodes))
  y_draw  <- nodes$y
  idx     <- which(is_root)
  
  # optional stacking for coincident roots
  if (length(idx)) {
    stack_gap <- 0.04
    key <- paste0(sprintf("%.6f", nodes$x_offset[idx]), "_", sprintf("%.6f", nodes$z_offset[idx]))
    groups <- split(idx, key)
    for (g in groups) {
      if (length(g) > 1L) {
        ranks <- seq_len(length(g)) - (length(g) + 1) / 2
        y_draw[g] <- y_draw[g] + ranks * stack_gap
      }
    }
  }
  
  # radial remap
  cx <- nodes$x_offset; cz <- nodes$z_offset
  dx <- nodes$x - cx;   dz <- nodes$z - cz
  r  <- sqrt(dx*dx + dz*dz)
  grp  <- paste0(round(cx, 6), "_", round(cz, 6))
  rmax <- ave(r, grp, FUN = function(v) max(v, 1e-9))
  rn   <- pmin(pmax(r / rmax, 0), 1)
  
  if (!isTRUE(all.equal(radial_expand, 1)) || !isTRUE(all.equal(radial_gamma, 1))) {
    s <- radial_expand * (rn^radial_gamma) / pmax(rn, 1e-9)
    nodes$x <- cx + dx * s
    nodes$z <- cz + dz * s
  }
  
  # propagate final node positions
  stopifnot(all(c("parent_path","child_path") %in% names(edges)),
            "path" %in% names(nodes))
  
  nodes$path        <- as.character(nodes$path)
  edges$parent_path <- as.character(edges$parent_path)
  edges$child_path  <- as.character(edges$child_path)
  
  ip <- match(edges$parent_path, nodes$path)
  ic <- match(edges$child_path,  nodes$path)
  if (anyNA(ip) || anyNA(ic)) stop("Some edge endpoints could not be matched to node paths.")
  
  edges$x     <- nodes$x[ip];  edges$z     <- nodes$z[ip];  edges$y     <- y_draw[ip]
  edges$x_end <- nodes$x[ic];  edges$z_end <- nodes$z[ic];  edges$y_end <- y_draw[ic]
  
  # keep final stacked/elevated y for consumers (post-labeling helpers)
  nodes$y_draw <- y_draw
  
  stems <- idx[abs(y_draw[idx] - nodes$y[idx]) > 1e-9]
  if (length(stems)) {
    segs <- as.numeric(t(cbind(
      nodes$x[stems], nodes$y[stems], nodes$z[stems],
      nodes$x[stems], y_draw[stems],  nodes$z[stems]
    )))
    rgl::segments3d(segs, color = "grey50", alpha = 0.5, depth_mask = FALSE, lwd = 2)
  }
  
  if (nrow(edges)) {
    edges$width_binned <- round(edges$width, 2)
    edges$alpha_binned <- round(edges$alpha, 3)
    styles <- unique(edges[c("width_binned","alpha_binned","color")])
    for (i in seq_len(nrow(styles))) {
      st <- styles[i, ]
      edge_idx <- edges$width_binned == st$width_binned &
        edges$alpha_binned == st$alpha_binned &
        edges$color == st$color
      sub <- edges[edge_idx, , drop = FALSE]
      coords <- as.numeric(t(cbind(sub$x, sub$y, sub$z, sub$x_end, sub$y_end, sub$z_end)))
      if (!is.null(th$materials$edges)) do.call(rgl::material3d, th$materials$edges)
      rgl::segments3d(coords, color = st$color, alpha = st$alpha_binned,lwd = st$width_binned, depth_mask = FALSE)
    }
  }
  
  if (!is.null(th$materials$nodes)) do.call(rgl::material3d, th$materials$nodes)
  r_draw <- nodes$radius * node_scale
  rgl::spheres3d(nodes$x, y_draw, nodes$z, radius = r_draw, color = node_cols)
  
  
  if (label_mode != "none") {
    
    # --- derive label text ---------------------------------------------------
    txt <- rep("", nrow(nodes))
    
    if (label_mode %in% c("interval", "interval_short", "item")) {
      txt <- if (label_mode == "interval" && "interval_label" %in% names(nodes)) {
        nodes$interval_label
      } else if (label_mode == "interval_short" && "interval_label_short" %in% names(nodes)) {
        nodes$interval_label_short
      } else {
        nodes$item
      }
    }
    
    if (label_mode == "bin") {
      # Numeric nodes prefer precomputed bin_index from build_coral_plots()
      # Fallback to interval_brackets if index missing.
      # robust numeric/non-numeric split
      has_kind <- "kind" %in% names(nodes)
      is_num <- if (has_kind) (!is.na(nodes$kind) & nodes$kind == "numeric")
      else grepl("\\[|\\(", nodes$item %||% "")
      
      if ("bin_index" %in% names(nodes)) {
        txt[is_num & !is.na(nodes$bin_index)] <- as.character(nodes$bin_index[is_num & !is.na(nodes$bin_index)])
      }
      # fallback: use bracket strings as labels if no index available
      if ("interval_brackets" %in% names(nodes)) {
        need <- is_num & (is.na(txt) | !nzchar(txt))
        txt[need] <- nodes$interval_brackets[need]
      }
      
      
      
      if (any(!is_num)) {
        if (label_non_numeric == "category" && "category_val" %in% names(nodes)) {
          # sanitize values once
          cat_raw <- as.character(nodes$category_val)
          cat_raw <- sub("^=+\\s*", "", cat_raw)
          # subset to the exact positions
          idx <- which(!is_num)
          txt[idx] <- ifelse(is.na(cat_raw[idx]), "", cat_raw[idx])
        } else if (label_non_numeric == "item") {
          txt[!is_num] <- nodes$item[!is_num]
        } else {
          txt[!is_num] <- ""
        }
      }
      
      # optional roman numerals
      if (identical(bin_label_fmt, "roman")) {
        ok <- is.finite(suppressWarnings(as.numeric(txt)))
        txt[ok] <- as.character(utils::as.roman(as.integer(txt[ok])))
      }
    }
    
    # roots must show *something*
    is_root <- if ("step" %in% names(nodes)) nodes$step == 0L else rep(FALSE, nrow(nodes))
    missing_root_txt <- (is.na(txt) | !nzchar(txt)) & is_root
    txt[missing_root_txt] <- nodes$item[missing_root_txt]
    
    # cap labels: keep largest radii + all roots
    keep <- if (isTRUE(max_labels <= 0)) which(is_root) else {
      ord <- order(nodes$radius, decreasing = TRUE)
      sort(unique(c(head(ord, max_labels), which(is_root))))
    }
    
    # place labels using the already-stacked y positions
    y_label <- nodes$y_draw - r_draw * label_offset
    
    # color override (vector or scalar); default to node colors
    lbl_cols <- if (!is.null(label_color)) {
      if (length(label_color) == 1L) rep(label_color, nrow(nodes)) else label_color
    } else {
      if ("color" %in% names(nodes)) nodes$color else "black"
    }
    
    lbl_mat <- th$materials$labels %||% list()
    if (is.null(lbl_mat$depth_test)) lbl_mat$depth_test <- "always"
    do.call(rgl::material3d, lbl_mat)
    
    rgl::text3d(
      x = nodes$x[keep],
      y = y_label[keep],
      z = nodes$z[keep],
      texts     = txt[keep],
      cex       = label_cex,
      color     = lbl_cols[keep],
      fixedSize = TRUE,
      adj       = c(0.5, 0.5)
    )
    rgl::material3d(depth_test = "less")
  }
  
  
  rgl::par3d(skipRedraw = FALSE)
  
  if (isTRUE(legend) && "color" %in% names(nodes)) {
    .dedupe <- function(df, cols) df[!duplicated(df[cols]), cols, drop = FALSE]
    
    if (legend_style == "auto") {
      has_bins <- !is.null(bin_legend) ||
        ("bin_index" %in% names(nodes) && any(is.finite(nodes$bin_index))) ||
        (!is.null(bin_breaks))
      legend_style <- if (has_bins) "feature_bins" else if (identical(node_color_by, "type")) "feature" else "grouped"
    }
    
    rgl::bgplot3d({
      op <- par(mar = c(0,0,0,0)); on.exit(par(op), add = TRUE)
      plot.new()
      
      .fit_text <- function(s, max_w, cex) {
        s <- as.character(s); if (!nzchar(s) || !is.finite(max_w) || max_w <= 0) return(s)
        if (graphics::strwidth(s, cex = cex) <= max_w) return(s)
        lo <- 1L; hi <- nchar(s); best <- "..."
        while (lo <= hi) {
          mid  <- (lo + hi) %/% 2L
          cand <- paste0(substr(s, 1L, mid), "...")
          if (graphics::strwidth(cand, cex = cex) <= max_w) { best <- cand; lo <- mid + 1L } else { hi <- mid - 1L }
        }
        best
      }
      
      anchor <- switch(legend_pos,
                       inside_right = c(0.9, 0.9),
                       topright     = c(0.98, 0.98),
                       topleft      = c(0.02, 0.98),
                       custom       = legend_xy)
      x_right <- anchor[1]; y_top <- anchor[2]
      
      panel_w <- legend_panel_width
      panel_h <- 1
      x_left  <- x_right - panel_w
      y_bot   <- y_top   - panel_h                      # <-- define y_bot
      graphics::rect(x_left, y_bot, x_right, y_top,
           col = rgb(1,1,1,0.88), border = NA, xpd = NA)
      
      title_cex <- if (is.null(legend_title_cex)) 1.00 * legend_cex else legend_title_cex
      row_cex   <- if (is.null(legend_row_cex))   0.95 * legend_cex else legend_row_cex
      
      # ---- shared geometry for all legend styles --------------------------------
      line_h   <- max(0.0175, graphics::strheight("M", cex = max(title_cex, row_cex)) * 1.25)
      top_pad  <- 0.50 * line_h                         # <-- define pads
      bot_pad  <- 0.30 * line_h
      usable_h <- panel_h - top_pad - bot_pad
      
      # inner columns (right to left) inside fixed panel
      ncol_max <- 2L
      col_pad  <- 0.004#max(0, legend_col_gap) / 2
      col_w    <- panel_w / ncol_max
      col_idx  <- 0L
      
      # rightmost column bounds and positions
      x_col_left  <- x_right - (col_idx + 1L) * col_w + col_pad
      x_col_right <- x_right -  col_idx       * col_w - col_pad
      
      # swatch + text positions sized to column
      sw_w    <- min(0.01, col_w * 0.32)
      txt_gap <- min(0.006, col_w * 0.12)
      x_sw_l  <- x_col_left
      x_sw_r  <- x_sw_l + sw_w
      x_txt   <- x_sw_r + txt_gap
      y       <- y_top - top_pad
      rows_per_col <- max(1L, floor(usable_h / line_h))
      
      if (legend_style == "feature") {
        stopifnot("feature" %in% names(nodes))
        L <- .dedupe(nodes, c("feature","color"))
        L <- L[order(L$feature), , drop = FALSE]
        # two columns if many
        n <- nrow(L); cols <- if (n > 12) 2L else 1L
        per_col <- ceiling(n / cols)
        col_w <- panel_w / cols
        
        k <- 1L
        for (cix in seq_len(cols)) {
          y <- y_top - line_h * 0.5
          x_col_right <- x_right - (cix - 1) * col_w
          x_sw   <- x_col_right - 0.025
          x_txt  <- x_col_right - 0.030
          for (i in seq_len(per_col)) {
            if (k > n) break
            graphics::rect(xleft = x_sw, ybottom = y - line_h * 0.6,
                 xright = x_col_right - 0.005, ytop = y - line_h * 0.15,
                 col = L$color[k], border = NA, xpd = NA)
            graphics::text(x_txt, y - line_h * 0.38, labels = L$feature[k],
                 cex = 0.85 * legend_cex, adj = c(1, 0.5))
            y <- y - line_h
            k <- k + 1L
          }
        }
        
      } else if (legend_style == "feature_bins") {
        # --- Build BL (bins) with fallbacks --------------------------------------
        BL <- bin_legend
        if (is.null(BL) && all(c("bin_index","interval_brackets","feature") %in% names(nodes))) {
          ok <- is.finite(nodes$bin_index) &
            !is.na(nodes$feature) & nzchar(nodes$feature) &
            !is.na(nodes$interval_brackets) & nzchar(nodes$interval_brackets)
          if (any(ok)) {
            BL <- unique(data.frame(
              feature  = as.character(nodes$feature[ok]),
              bin      = as.integer(nodes$bin_index[ok]),
              interval = as.character(nodes$interval_brackets[ok]),
              stringsAsFactors = FALSE
            ))
          }
        }
        if (is.null(BL) && !is.null(bin_breaks)) {
          mk <- function(f, br) {
            br <- sort(unique(as.numeric(br))); if (length(br) < 2) return(NULL)
            data.frame(feature=f,
                       bin=seq_len(length(br)-1L),
                       interval=sprintf("[%s,%s)", format(br[-length(br)]), format(br[-1L])),
                       stringsAsFactors=FALSE)
          }
          L <- Filter(Negate(is.null), lapply(names(bin_breaks), function(f) mk(f, bin_breaks[[f]])))
          if (length(L)) BL <- do.call(rbind, L)
        }
        if (is.null(BL) || !nrow(BL)) return(invisible())
        
        BL$feature  <- as.character(BL$feature)
        BL$interval <- as.character(BL$interval)
        
        # --- colors from drawn nodes (dataset-agnostic via normalized keys) ------
        norm_key <- function(s) gsub("[^[:alnum:]]+", "", tolower(trimws(as.character(s))))
        dd <- data.frame(key = norm_key(nodes$feature),
                         color = as.character(nodes$color),
                         stringsAsFactors = FALSE)
        dd <- dd[nzchar(dd$key) & nzchar(dd$color) & !is.na(dd$key) & !is.na(dd$color), , drop = FALSE]
        dd <- dd[!duplicated(dd$key), , drop = FALSE]
        feat_cols <- as.list(setNames(dd$color, dd$key))  # lookup by normalized key
        
        # --- Build draw items: numerics first, then categorical summary ----------
        feats_num <- sort(unique(BL$feature))
        if (length(feats_num) > legend_features_max) feats_num <- feats_num[seq_len(legend_features_max)]
        
        items <- list()  # each item: list(title, rows, color, need)
        add_item <- function(title, rows, col) {
          items[[length(items) + 1L]] <<- list(title = title, rows = rows, col = col,
                                               need = 1L + length(rows))
        }
        
        # numeric items
        for (f in feats_num) {
          rows <- BL[BL$feature == f, , drop = FALSE]
          rows <- rows[order(rows$bin), , drop = FALSE]
          blab <- if (identical(bin_label_fmt, "roman")) as.character(utils::as.roman(rows$bin)) else as.character(rows$bin)
          txt  <- sprintf("%s  %s", blab, rows$interval)
          add_item(f, txt, feat_cols[[norm_key(f)]] %||% "grey70")
        }
        
        # categorical items (1 row per feature) - appended after numerics
        cats_all  <- if ("kind" %in% names(nodes)) sort(unique(as.character(nodes$feature[nodes$kind == "categorical"]))) else character(0)
        cats_show <- setdiff(cats_all, unique(BL$feature))
        for (f in cats_show) {
          vals <- unique(na.omit(as.character(nodes$category_val[nodes$feature == f])))
          vals <- gsub("^=+\\s*", "", vals); vals <- vals[nzchar(vals)]
          lbl  <- if (length(vals) == 1L) sprintf("%s = %s", f, vals[1]) else sprintf("%s in {%s}", f, paste(sort(vals), collapse = ", "))
          add_item(f, lbl, feat_cols[[norm_key(f)]] %||% "grey70")
        }
        if (!length(items)) return(invisible())
        
        # --- decide #columns and set per-column geometry -------------------------
        total_lines <- sum(vapply(items, `[[`, integer(1), "need"))
        cap_per_col <- max(1L, floor(usable_h / line_h))
        ncol_needed <- max(1L, ceiling(total_lines / cap_per_col))
        ncol_max    <- min(max(2L, ncol_needed), 4L)   # allow 2..4 columns
        
        # per-column geometry (fixed for this draw) - RIGHT-ALIGNED
        col_w  <- panel_w / ncol_max
        sw_w   <- min(0.01, col_w * 0.32)
        txt_gp <- min(0.008, col_w * 0.22)
        
        # right to left columns: index 1..ncol_max (1 is rightmost)
        col_left  <- x_right - (1:ncol_max) * col_w + col_pad
        col_right <- x_right - (0:(ncol_max-1)) * col_w - col_pad
        
        # anchor at right edge of each column; text grows LEFT, swatch sits left of text
        x_sw_l_v <- col_left  + 0.004
        x_sw_r_v <- x_sw_l_v + sw_w
        x_txt_v  <- x_sw_r_v + txt_gp
        
        # per-column y cursors (start at the top)
        y_v <- rep(y_top - top_pad, ncol_max)
        
        # advance to next column when current column has no room for `need` rows
        cur <- 1L
        advance <- function(need_rows) {
          while (cur <= ncol_max && (y_v[cur] - need_rows * line_h) < (y_bot + bot_pad)) {
            cur <<- cur + 1L
          }
          cur <= ncol_max
        }
        
        draw_block <- function(title, rows_vec, color_hex) {
          need <- 1L + length(rows_vec)
          if (!advance(need)) return(FALSE)
          
          x_txt  <- x_txt_v[cur]
          x_sw_l <- x_sw_l_v[cur]
          x_sw_r <- x_sw_r_v[cur]
          y      <- y_v[cur]
          
          # title (right-aligned)
          #title_cex <- 1.00 * legend_cex
          max_w     <- col_right[cur] - 0.004 - x_txt
          title_lab <- .fit_text(title, max_w, title_cex)
          graphics::text(x_txt, y, labels = title_lab,
               cex = title_cex, adj = c(0, 1), font = 2)
          y <- y - line_h
          
          # rows (right-aligned; swatch to the left)
          if (length(rows_vec)) {
            rv <- if (is.character(rows_vec)) rows_vec else as.character(rows_vec)
            for (i in seq_along(rv)) {
              graphics::rect(xleft = x_sw_l, ybottom = y - 0.60 * line_h,
                   xright = x_sw_r, ytop = y - 0.15 * line_h,
                   col = color_hex, border = NA, xpd = NA)
              lab <- .fit_text(rv[i], col_right[cur] - 0.004 - x_txt, row_cex)
              graphics::text(x_txt, y - 0.38 * line_h, labels = lab,
                   cex = row_cex, adj = c(0, 0.5))
              y <- y - line_h
            }
          }
          
          y <- y - 0.40 * line_h
          y_v[cur] <<- y
          TRUE
        }
        
        # --- draw items sequentially: numerics first, then categorical -----------
        for (it in items) {
          draw_block(it$title, it$rows, it$col)
        }
      }
      else {  # legend_style == "grouped"
        need <- c("feature","item","color")
        if (!all(need %in% names(nodes))) stop("nodes must have columns: feature, item, color")
        df <- .dedupe(nodes, need)
        df <- df[order(df$feature, df$item), , drop = FALSE]
        split_by_feat <- split(df, df$feature, drop = TRUE)
        # order features by how many items we can show (desc), then alpha
        cnt <- vapply(split_by_feat, nrow, integer(1))
        nm  <- names(split_by_feat)
        ord <- order(-cnt, nm)
        split_by_feat <- split_by_feat[ord]
        if (length(split_by_feat) > legend_features_max) {
          split_by_feat <- split_by_feat[seq_len(legend_features_max)]
        }
        # draw
        for (feat in names(split_by_feat)) {
          graphics::text(x_txt, y, labels = feat, cex = 0.95 * legend_cex, adj = c(1, 1), font = 2)
          y <- y - line_h
          d <- head(split_by_feat[[feat]], legend_items_per_feature)
          for (i in seq_len(nrow(d))) {
            graphics::rect(xleft = x_sw, ybottom = y - line_h * 0.6,
                 xright = x_right - 0.005, ytop = y - line_h * 0.15,
                 col = d$color[i], border = NA, xpd = NA)
            # prefer a short interval label if available
            lab <- d$item[i]
            if ("interval_label_short" %in% names(nodes)) {
              alt <- nodes$interval_label_short[match(lab, nodes$item)]
              if (length(alt) && !is.na(alt[1]) && nzchar(alt[1])) lab <- alt[1]
            }
            graphics::text(x_txt, y - line_h * 0.38, labels = lab, cex = 0.80 * legend_cex, adj = c(1, 0.5))
            y <- y - line_h
            if (y < 0.06) break
          }
          y <- y - line_h * 0.4
          if (y < 0.06) break
        }
      }
      
    })
  }
  
  if (return_data) return(invisible(list(nodes = nodes, edges = edges)))
  invisible(NULL)
}