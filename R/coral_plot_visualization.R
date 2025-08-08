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
    grid_color = "grey80",
    legend = FALSE,
    label_mode = c("none","interval","item","interval_short"),
    label_cex  = 0.7,
    label_offset = 1.5,
    max_labels = 100
) {
  label_mode <- match.arg(label_mode)
  
  open3d()
  par3d(windowRect = c(50, 50, 1000, 1000))
  
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  view3d(theta = 0, phi = phi_deg, fov = 60)
  
  aspect3d(1, 1, 1)
  par3d(skipRedraw = TRUE)
  
  # grid
  xlim <- c(0, grid_size); zlim <- c(0, grid_size)
  xs   <- seq(xlim[1], xlim[2], by = 1)
  zs   <- seq(zlim[1], zlim[2], by = 1)
  for (z in zs) lines3d(x = xlim, y = 0,   z = c(z,z), color = grid_color)
  for (x in xs) lines3d(x = x,    y = c(0,0), z = zlim, color = grid_color)
  #lines3d(x = xlim, y = 0, z = 0,   col = "red",  lwd = 4)
  #lines3d(x = 0,    y = 0, z = zlim, col = "blue", lwd = 4)
  
  # edges
  if (all(c("color","width") %in% names(edges))) {
    styles <- dplyr::distinct(edges, color, width)
    for (i in seq_len(nrow(styles))) {
      st  <- styles[i, ]
      sub <- dplyr::filter(edges, color == st$color & width == st$width)
      coords <- as.numeric(t(as.matrix(sub[, c("x","y","z","x_end","y_end","z_end")])))
      segments3d(coords, color = st$color, lwd = st$width, alpha = 0.6)
    }
  }
  
  # nodes
  node_cols <- if ("color" %in% names(nodes)) {
    col <- as.character(nodes$color); col[is.na(col) | !nzchar(col)] <- "black"; col
  } else "black"
  spheres3d(nodes$x, nodes$y, nodes$z, radius = nodes$radius, color = node_cols)
  
  # labels
  if (label_mode != "none") {
    base_txt <- if (label_mode == "interval" && "interval_label" %in% names(nodes)) {
      nodes$interval_label
    } else if (label_mode == "interval_short" && "interval_label_short" %in% names(nodes)) {
      nodes$interval_label_short
    } else {
      nodes$item
    }
    
    # detect RHS/root nodes
    if ("step" %in% names(nodes)) {
      is_root <- nodes$step == 1L
    } else if (all(c("x_offset","z_offset") %in% names(nodes))) {
      is_root <- (abs(nodes$x - nodes$x_offset) < 1e-9) & (abs(nodes$z - nodes$z_offset) < 1e-9)
    } else {
      is_root <- FALSE
    }
    
    # main keep-set: biggest nodes by draw radius
    ord       <- order(nodes$radius, decreasing = TRUE)
    keep_main <- head(ord, max_labels)
    
    # always include roots
    keep_roots <- which(is_root)
    
    # if a root has empty text under current mode, fall back to item
    txt <- base_txt
    if (length(keep_roots)) {
      missing_root_txt <- is.na(txt) | !nzchar(txt)
      txt[ missing_root_txt & is_root ] <- nodes$item[ missing_root_txt & is_root ]
    }
    
    # final keep-set
    keep <- sort(unique(c(keep_main, keep_roots)))
    
    # draw labels (below nodes; screen-size constant; on top)
    rgl::material3d(depth_test = "always")
    rgl::text3d(
      x = nodes$x[keep],
      y = nodes$y[keep] - nodes$radius[keep] * label_offset,
      z = nodes$z[keep],
      texts     = txt[keep],
      cex       = label_cex,
      color     = node_cols[keep],
      fixedSize = TRUE,
      adj       = c(0.5, 1)
    )
    rgl::material3d(depth_test = "less")
  }
  
  par3d(skipRedraw = FALSE)
  
  if (legend && "color" %in% names(nodes)) {
    # distinct label-color pairs
    if ("type" %in% names(nodes)) {
      legend_df <- dplyr::distinct(nodes, label = .data$type, color)
    } else if ("item" %in% names(nodes)) {
      legend_df <- dplyr::distinct(nodes, label = .data$item, color)
    } else {
      legend_df <- NULL
    }
    
    if (!is.null(legend_df) && nrow(legend_df) > 0) {
      # draw in the margin of the rgl window
      rgl::bgplot3d({
        op <- par(mar = c(0,0,0,0))
        plot.new()
        legend(
          "topright",
          legend = legend_df$label,
          fill   = legend_df$color,
          border = NA,
          bty    = "n",
          cex    = 0.8
        )
        par(op)
      })
    }
  }
}
