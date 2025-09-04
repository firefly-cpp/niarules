#' @title Apply styling to coral plots and render them with rgl
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
#'   or the theme’s grid color). Defaults to `FALSE` for clean screenshots.
#' @param grid_color color for the grid (if `grid_outline = TRUE`). If missing, the active
#'   theme’s grid color is used. Default `"grey92"`.
#' @param legend logical; draw a node legend keyed by base feature (`nodes$feature`).
#'   Requires that `nodes$feature` and node colors are available. Default `FALSE`.
#' @param label_mode one of `"none"`, `"interval"`, `"item"`, `"interval_short"`.
#'   Controls label text: interval labels, item labels, or no labels.
#' @param label_cex numeric; label size passed to `rgl::text3d()`. Default `0.7`.
#' @param label_offset numeric; vertical offset (in **node radii**) applied
#'   to labels (positive values move labels downward from sphere tops). Default `1.5`.
#' @param max_labels integer; maximum number of non-root labels (largest radii first).
#'   Root nodes are always kept. If `<= 0`, only root (RHS) labels are drawn.
#' @param label_color `NULL` to color labels like their nodes, or a single color / vector to override.
#'
#' @param theme character; one of `"default"`, `"studio"`, `"flat"`, `"dark"`, `"none"`.
#'   Selects a preset for lights, materials, and background:
#'   - **default**: balanced lighting with subtle specular highlights.
#'   - **studio**: brighter, glossy look for screenshots.
#'   - **flat**: low-specular, diagram-style shading.
#'   - **dark**: dark background with rim lighting.
#'   - **none**: no lights configured (use existing rgl state/ambient).
#' @param theme_overrides optional named list to partially override the selected theme.
#'   Supported keys: `background` (color), `grid_color` (color),
#'   `materials` (list with sublists `nodes`, `edges`, `labels` — each passed to
#'   [rgl::material3d()]), and `lights` (list of argument lists for [rgl::light3d()]).
#'   Example: `list(lights = list(list(theta = 60, phi = 30)))`.
#' @param apply_theme logical; if `TRUE` (default) the theme is applied at the start of rendering
#'   (background, lights, global/material defaults). Set to `FALSE` to keep the current rgl device
#'   state (useful when you configure lights/materials once for batch rendering).
#'
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
#'   - `"edge_incoming"` uses the mean of incoming edges’ normalized color metric (`t_color_norm`).
#'   - `"edge_outgoing_mean"` uses the mean of outgoing edges’ normalized color metric.
#'   Default `"type"`.
#' @param node_gradient either the string `"match"` to reuse `edge_gradient` for nodes,
#'   or a character vector (>= 2) of colors to build the node palette. Default `"match"`.
#' @param node_gradient_map one of `"even"`, `"hash"`, `"frequency"`; how unique labels are
#'   placed along the gradient:
#'   - `"even"`: evenly spaced by sorted unique label order,
#'   - `"hash"`: stable per-label positions via a lightweight hash (reproducible),
#'   - `"frequency"`: labels ordered by frequency (most frequent near one end).
#'   Default `"even"`.
#'
#' @param radial_expand numeric; global expand/contract factor applied radially
#'   from each plot center. `1` = no change; `>1` pushes nodes outward. Default `1`.
#' @param radial_gamma numeric; curvature of the radial remap. `1` = linear,
#'   `>1` separates outer rings (more spacing near the edge), `<1` compresses them.
#'   Default `1`.
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
#' Metric scaling uses a helper that:
#' 1) rescales the chosen metric to `[0,1]` over finite values, and
#' 2) applies the selected transform:
#'    - `"linear"`: identity,
#'    - `"sqrt"`: emphasizes differences at the low end,
#'    - `"log"`: `log1p(9*t)/log(10)`, emphasizing very small values.
#'
#' Node elevation (`y`) is computed as `y_scale * r_norm` where `r_norm` is the node’s
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
render_coral_rgl <- function(
    nodes, edges,
    
    grid_size,
    grid_outline = FALSE,
    grid_color   = "grey92",
    
    legend       = FALSE,
    
    label_mode   = c("none", "interval", "item", "interval_short"),
    label_cex    = 0.7,
    label_offset = 1.5,
    max_labels   = 100,
    label_color  = NULL,
    
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
    
    radial_expand = 1.0,
    radial_gamma  = 1.0,
    
    y_scale = 0,        # << 0 keeps current "flat" look; try 0.5–0.8
    jitter_sd    = 0.0,
    jitter_mode  = c("deterministic","random"),
    jitter_seed  = NULL,
    
    return_data = FALSE
) {
  
  # ensure a device exists so apply_theme doesn't create one
  if (rgl::rgl.cur() == 0) rgl::open3d()
  
  theme <- match.arg(theme)
  th <- coral_get_theme(theme, theme_overrides)
  if (missing(grid_color) && !is.null(th$grid_color)) grid_color <- th$grid_color
  if (isTRUE(apply_theme)) coral_apply_theme(th)
  
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
  rgl::par3d(windowRect = c(50, 50, 1000, 1000))
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  rgl::view3d(theta = 0, phi = phi_deg, fov = 60)
  rgl::aspect3d(1, 1, 1)
  rgl::par3d(skipRedraw = TRUE)
  
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
      rgl::segments3d(coords, color = st$color, alpha = st$alpha_binned,
                      lwd = st$width_binned, depth_mask = FALSE)
    }
  }
  
  if (!is.null(th$materials$nodes)) do.call(rgl::material3d, th$materials$nodes)
  rgl::spheres3d(nodes$x, y_draw, nodes$z, radius = nodes$radius, color = node_cols)
  
  if (label_mode != "none") {
    txt <- if (label_mode == "interval" && "interval_label" %in% names(nodes)) {
      nodes$interval_label
    } else if (label_mode == "interval_short" && "interval_label_short" %in% names(nodes)) {
      nodes$interval_label_short
    } else {
      nodes$item
    }
    
    # make sure roots always have some text
    missing_root_txt <- (is.na(txt) | !nzchar(txt)) & is_root
    txt[missing_root_txt] <- nodes$item[missing_root_txt]
    
    # - max_labels <= 0  -> show only roots
    # - else             -> top-N by radius plu all roots
    if (isTRUE(max_labels <= 0)) {
      keep <- which(is_root)
    } else {
      ord <- order(nodes$radius, decreasing = TRUE)
      keep_main <- head(ord, max_labels)
      keep <- sort(unique(c(keep_main, which(is_root))))
    }
    
    # place labels using the already-stacked y positions
    y_label <- y_draw - nodes$radius * label_offset
    
    # optional color override (vector or scalar); default to node colors
    lbl_cols <- if (!is.null(label_color)) {
      if (length(label_color) == 1L) rep(label_color, nrow(nodes)) else label_color
    } else {
      node_cols
    }
    
    lbl_mat <- th$materials$labels %||% list()
    # respect an explicit theme override if the user really wants different behavior;
    # otherwise default to "always" so labels stay in front.
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
      adj       = c(0.5, 1)
    )
    
    # restore the normal depth test so later geometry behaves as expected
    rgl::material3d(depth_test = "less")
  }
  
  
  rgl::par3d(skipRedraw = FALSE)
  
  if (legend && "color" %in% names(nodes)) {
    if (!"feature" %in% names(nodes)) stop("nodes$feature missing; legend needs base feature names.")
    legend_df <- unique(nodes[c("feature","color")])
    names(legend_df) <- c("label","color")
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