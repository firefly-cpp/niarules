#' @title applies styling to coral plots and renders them using rgl
#'
#' @description
#' Expects nodes/edges from build_coral_layout(). Edges must contain
#' columns: x,y,z,x_end,y_end,z_end and metrics: support, confidence, lift.
#'
#' @param nodes data.frame from build_coral_layout()$nodes
#' @param edges data.frame from build_coral_layout()$edges
#' @param grid_size integer from build_coral_layout()$grid_size
#' @param grid_color background grid color
#' @param legend show a node legend (by base feature)
#' @param label_mode "none"|"interval"|"item"|"interval_short"
#' @param label_cex label size
#' @param label_offset vertical offset in radii
#' @param max_labels max non-root labels to be shown
#' @param edge_width_metric which metric to map to edge width
#' @param edge_color_metric which metric to map to edge width
#' @param edge_alpha_metric which metric to map to edge width
#' @param edge_width_range numeric length-2, min/max lwd
#' @param edge_width_transform "linear"|"sqrt"|"log"
#' @param edge_gradient character vector (>=2) for edge colors
#' @param edge_color_transform "linear"|"sqrt"|"log"
#' @param edge_alpha edge transparency (0..1)
#' @param edge_alpha_range TODO
#' @param edge_alpha_transform "linear"|"sqrt"|"log"
#' @param node_color_by TODO
#' @param node_gradient TODO
#' @param node_gradient_map TODO
#' @param return_data used for testing only
#' @param y_scale numeric scalar; vertical scale (0 = flat, default).
#' @param jitter_sd TODO
#' @param jitter_mode TODO
#' @param jitter_seed TODO,
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
    edge_alpha          = 0.6,
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
  edges$color <- grDevices::rgb(rgb[,1], rgb[,2], rgb[,3], alpha = a, maxColorValue = 255)
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
  # 1) base radial distance and per-plot normalization (so max radius -> 1)
  r <- sqrt((nodes$x - nodes$x_offset)^2 + (nodes$z - nodes$z_offset)^2)
  key_center <- paste0(sprintf("%.6f", nodes$x_offset), "_", sprintf("%.6f", nodes$z_offset))
  r_max <- ave(r, key_center, FUN = function(v) if (length(v) && max(v) > 0) max(v) else 1)
  r_norm <- ifelse(r_max > 0, r / r_max, 0)
  
  # 2) base y
  nodes$y <- y_scale * r_norm
  
  # 3) jitter that fades to 0 at the center (multiplied by r_norm)
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
  
  if (nrow(edges)) {
    edges$width_binned <- round(edges$width, 2)
    styles <- unique(edges[c("color","width_binned")])
    for (i in seq_len(nrow(styles))) {
      st  <- styles[i, ]
      sub <- edges[edges$color == st$color & edges$width_binned == st$width_binned, ]
      coords <- as.numeric(t(cbind(sub$x, sub$y, sub$z,
                                   sub$x_end, sub$y_end, sub$z_end)))
      rgl::segments3d(coords, color = st$color, lwd = st$width_binned, alpha = 1)
    }
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
  
  stems <- idx[abs(y_draw[idx] - nodes$y[idx]) > 1e-9]
  if (length(stems)) {
    segs <- as.numeric(t(cbind(
      nodes$x[stems], nodes$y[stems], nodes$z[stems],
      nodes$x[stems], y_draw[stems],  nodes$z[stems]
    )))
    rgl::segments3d(segs, color = "grey50", alpha = 0.5, lwd = 2)
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

# Fill colors for a set of labels, honoring user overrides.
# - labels: character vector (usually unique labels)
# - user_map: optional named vector of colors, names must match labels
# - hcl_c / hcl_l: chroma & luminance for auto HCL palette
.coral_auto_fill_named_colors <- function(labels, user_map = NULL, hcl_c = 80, hcl_l = 50) {
  labels <- unique(as.character(labels))
  # start with NA, keep names = labels
  out <- setNames(rep(NA_character_, length(labels)), labels)
  
  # apply user overrides where names match exactly
  if (!is.null(user_map)) {
    user_map <- as.character(user_map)
    hit <- intersect(names(user_map), labels)
    if (length(hit)) out[hit] <- user_map[hit]
  }
  
  # auto-fill the rest with evenly spaced HCL hues
  miss <- names(out)[is.na(out) | !nzchar(out)]
  if (length(miss)) {
    k <- length(miss)
    hues <- seq(15, 375, length.out = k + 1)[1:k]
    out[miss] <- grDevices::hcl(h = hues, c = hcl_c, l = hcl_l)
  }
  
  out  # <- KEEP NAMES
}
