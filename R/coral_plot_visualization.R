#' @title Render a Coral Plot in 3D Using rgl
#'
#' @description
#' Renders a coral network layout in an interactive 3D plot using the \pkg{rgl} package.
#' This includes a customizable grid, edges between nodes (with optional styling), and nodes
#' themselves (with optional color coding based on features).
#'
#' @param nodes A data frame containing node positions and properties. Must include columns:
#'   \code{x}, \code{y}, \code{z}, \code{radius}, and \code{item}.
#' @param edges A data frame containing edge positions and styles. Must include columns:
#'   \code{x}, \code{y}, \code{z}, \code{x_end}, \code{y_end}, \code{z_end}, and optionally
#'   \code{color} and \code{width}.
#' @param grid_size Integer indicating the number of grid cells in each dimension of the layout.
#'   This determines the size and scale of the background grid.
#' @param feature_cols Optional named vector mapping node item names to colors. If provided, it is
#'   used to color the nodes accordingly. Defaults to \code{NULL} (black nodes).
#' @param grid_color Color of the background grid lines. Defaults to \code{"grey80"}.
#' @param legend Logical indicating whether to display a legend. Currently not implemented. Defaults to \code{FALSE}.
#'
#' @details
#' The function first opens a new rgl window, positions the 3D camera, and draws a grid on the x–z plane.
#' Edges are drawn using \code{segments3d()} and can be styled by width and color. Nodes are drawn
#' as 3D spheres via \code{spheres3d()}.
#'
#' The x and z axes are highlighted in red and blue, respectively, to assist orientation.
#'
#' @importFrom rgl open3d par3d aspect3d lines3d segments3d spheres3d view3d
#' @importFrom dplyr filter distinct
#' @export
render_coral_rgl <- function(
    nodes, edges, grid_size,
    grid_color   = "grey80",
    legend       = FALSE,
    label_mode   = c("none", "interval", "item", "interval_short"),
    label_cex    = 0.7,
    label_offset = 1.5,
    max_labels   = 100
) {
  label_mode <- match.arg(label_mode)
  
  rgl::open3d()
  rgl::par3d(windowRect = c(50, 50, 1000, 1000))
  
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  rgl::view3d(theta = 0, phi = phi_deg, fov = 60)
  
  rgl::aspect3d(1, 1, 1)
  rgl::par3d(skipRedraw = TRUE)
  
  # ---- grid ----
  xlim <- c(0, grid_size)
  zlim <- c(0, grid_size)
  xs   <- seq(xlim[1], xlim[2], by = 1)
  zs   <- seq(zlim[1], zlim[2], by = 1)
  for (z in zs) rgl::lines3d(x = xlim, y = 0, z = c(z, z), color = grid_color)
  for (x in xs) rgl::lines3d(x = x, y = c(0, 0), z = zlim, color = grid_color)
  
  # ---- edges ----
  if (!is.null(edges) && nrow(edges) && all(c("color", "width") %in% names(edges))) {
    styles <- dplyr::distinct(edges, color, width)
    for (i in seq_len(nrow(styles))) {
      st  <- styles[i, ]
      sub <- dplyr::filter(edges, color == st$color & width == st$width)
      coords <- as.numeric(t(as.matrix(sub[, c("x", "y", "z", "x_end", "y_end", "z_end")])))
      rgl::segments3d(coords, color = st$color, lwd = st$width, alpha = 0.6)
    }
  }
  
  # ---- nodes ----
  node_cols <- if ("color" %in% names(nodes)) {
    col <- as.character(nodes$color)
    col[is.na(col) | !nzchar(col)] <- "black"
    col
  } else {
    "black"
  }
  
  # Root detection from C++ step
  is_root <- ("step" %in% names(nodes)) & (nodes$step == 0L)
  y_draw  <- nodes$y
  idx     <- which(is_root)
  
  # Stack only if multiple roots share the same plot center
  if (length(idx)) {
    stack_gap <- 0.04
    # x_offset/z_offset are always present now
    key <- paste0(sprintf("%.6f", nodes$x_offset[idx]), "_",
                  sprintf("%.6f", nodes$z_offset[idx]))
    groups <- split(idx, key)
    for (g in groups) {
      if (length(g) > 1L) {
        ranks <- seq_len(length(g)) - (length(g) + 1) / 2
        y_draw[g] <- y_draw[g] + ranks * stack_gap
      }
    }
  }
  
  # Stems for stacked roots
  stems <- idx[abs(y_draw[idx] - nodes$y[idx]) > 1e-9]
  if (length(stems)) {
    segs <- as.numeric(t(cbind(
      nodes$x[stems], 0,               nodes$z[stems],
      nodes$x[stems], y_draw[stems],   nodes$z[stems]
    )))
    rgl::segments3d(segs, color = "grey50", alpha = 0.5, lwd = 2)
  }
  
  # Draw spheres with y_draw
  rgl::spheres3d(nodes$x, y_draw, nodes$z, radius = nodes$radius, color = node_cols)
  
  # ---- labels ----
  if (label_mode != "none") {
    txt <- if (label_mode == "interval" && "interval_label" %in% names(nodes)) {
      nodes$interval_label
    } else if (label_mode == "interval_short" && "interval_label_short" %in% names(nodes)) {
      nodes$interval_label_short
    } else {
      nodes$item
    }
    
    # Always keep roots, plus the top max_labels others by radius
    ord <- order(nodes$radius, decreasing = TRUE)
    keep_main <- head(ord, max_labels)
    keep <- sort(unique(c(keep_main, idx)))
    
    # ensure root text exists (should already, but belt & braces)
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
  
  # ---- legend ----
  if (legend && "color" %in% names(nodes)) {
    # color-by-feature is the usual case now
    legend_df <- if ("feature" %in% names(nodes)) {
      dplyr::distinct(nodes, label = .data$feature, color)
    } else if ("item" %in% names(nodes)) {
      dplyr::distinct(nodes, label = .data$item, color)
    } else {
      NULL
    }
    
    if (!is.null(legend_df) && nrow(legend_df) > 0) {
      rgl::bgplot3d({
        op <- par(mar = c(0,0,0,0))
        plot.new()
        legend("topright", legend = legend_df$label, fill = legend_df$color,
               border = NA, bty = "n", cex = 0.8)
        par(op)
      })
    }
  }
}