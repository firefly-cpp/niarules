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
#' @param edge_metric which metric to map to edge width/color
#' @param edge_width_range numeric length-2, min/max lwd
#' @param edge_width_transform "linear"|"sqrt"|"log"
#' @param edge_gradient character vector (>=2) for edge colors
#' @param edge_alpha edge transparency (0..1)
#' @param node_color_by "type" (base feature), "item" (full string), or "none"
#' @param node_colors optional named overrides for node colors
#' @param palette_hcl_c HCL palette params for auto node colors
#' @param palette_hcl_l HCL palette params for auto node colors
#' @param return_data used for testing only
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
    edge_metric  = c("confidence","lift","support"),
    edge_width_range = c(1, 5),
    edge_width_transform = c("linear","sqrt","log"),
    edge_gradient = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
    edge_alpha   = 0.6,
    node_color_by = c("type","item","none"),
    node_colors   = NULL,
    palette_hcl_c = 80,
    palette_hcl_l = 50,
    return_data = FALSE
) {
  stopifnot(is.data.frame(nodes), is.data.frame(edges))
  label_mode          <- match.arg(label_mode)
  node_color_by       <- match.arg(node_color_by)
  edge_metric         <- match.arg(edge_metric)
  edge_width_transform<- match.arg(edge_width_transform)
  
#### edge styling: width + color from chosen metric
  # TODO: different metrics for width and color
  
  if (!edge_metric %in% names(edges)) {
    stop(sprintf("edges is missing '%s' column; build_coral_layout must return it.", edge_metric))
  }
  m <- as.numeric(edges[[edge_metric]])
  rng <- range(m, finite = TRUE)
  if (!is.finite(rng[1]) || !(rng[2] > rng[1])) {
    t <- rep(0.5, length(m))
  } else {
    t <- (m - rng[1]) / (rng[2] - rng[1])
  }
  # optional transforms to spread dynamic range
  if (edge_width_transform == "sqrt") t <- sqrt(t)
  if (edge_width_transform == "log")  t <- log1p(t * 9) / log(10)  # 0..1
  t[!is.finite(t)] <- 0.5
  
  # widths
  edges$width <- edge_width_range[1] + t * (edge_width_range[2] - edge_width_range[1])
  
  # colors via gradient
  if (length(edge_gradient) < 2L) edge_gradient <- c("#2166AC","#B2182B")
  cr  <- grDevices::colorRamp(edge_gradient)
  rgb <- cr(t) # 0..255
  edges$color <- grDevices::rgb(rgb[,1], rgb[,2], rgb[,3], alpha = edge_alpha, maxColorValue = 255)
  
#### node colors
  #### node colors
  # choose the key used for coloring
  if (node_color_by == "type") {
    key <- as.character(nodes$feature)
  } else if (node_color_by == "item") {
    key <- as.character(nodes$item)
  } else {
    key <- NULL
  }
  
  if (!is.null(key)) {
    nodes$color <- NA_character_
    
    # 1) apply user overrides by direct equality on key
    if (!is.null(node_colors)) {
      if (is.null(names(node_colors)) || any(!nzchar(names(node_colors)))) {
        warning("node_colors must be a named character vector (names = labels).")
      } else {
        # ensure character with names preserved
        node_colors <- as.character(node_colors)
        for (nm in names(node_colors)) {
          hit <- which(key == nm)
          if (length(hit)) nodes$color[hit] <- node_colors[[nm]]
        }
      }
    }
    
    # 2) auto-fill remaining labels with HCL palette
    need <- is.na(nodes$color) & !is.na(key) & nzchar(key)
    if (any(need)) {
      uniq_missing <- sort(unique(key[need]))
      pal_missing  <- .coral_auto_fill_named_colors(uniq_missing, NULL, palette_hcl_c, palette_hcl_l)
      # pal_missing is named; map by the actual key values
      nodes$color[need] <- pal_missing[ key[need] ]
    }
    
    # 3) fallback for anything still NA
    nodes$color[is.na(nodes$color)] <- "black"
  }
  
  # open window & draw grid
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
  
  # draw edges (batch by color & width for speed)
  if (nrow(edges)) {
    edges$width_binned <- round(edges$width, 2)
    styles <- unique(edges[c("color","width_binned")])
    for (i in seq_len(nrow(styles))) {
      st  <- styles[i, ]
      sub <- edges[edges$color == st$color & edges$width_binned == st$width_binned, ]
      coords <- as.numeric(t(as.matrix(sub[, c("x","y","z","x_end","y_end","z_end")])))
      rgl::segments3d(coords, color = st$color, lwd = st$width_binned, alpha = 1)
    }
  }
  
  # draw nodes + stacking stems
  node_cols <- if ("color" %in% names(nodes)) {
    col <- as.character(nodes$color); col[is.na(col) | !nzchar(col)] <- "black"; col
  } else "black"
  
  is_root <- if ("step" %in% names(nodes)) nodes$step == 0L else rep(FALSE, nrow(nodes))
  y_draw  <- nodes$y
  idx     <- which(is_root)
  
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
      nodes$x[stems], 0,               nodes$z[stems],
      nodes$x[stems], y_draw[stems],   nodes$z[stems]
    )))
    rgl::segments3d(segs, color = "grey50", alpha = 0.5, lwd = 2)
  }
  
  rgl::spheres3d(nodes$x, y_draw, nodes$z, radius = nodes$radius, color = node_cols)
  
  # draw labels
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
  
  # draw legend
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
  
  if (return_data) return(invisible(list(nodes = nodes, edges = edges, edge_metric = edge_metric)))
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
