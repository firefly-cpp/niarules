#' Render radial plots in 3D
#'
#' TODO: full description here
#'
#' @param nodes TODO
#' @param edges TODO
#' @param grid_size TODO
#' @param feature_cols TODO
#' @param grid_color TODO
#' @param legend TODO
#'
#' @importFrom rgl open3d par3d aspect3d lines3d segments3d spheres3d view3d
#' @importFrom dplyr filter distinct
#' @export
render_radial_rgl <- function(
    nodes,
    edges,
    grid_size,
    feature_cols = NULL,
    grid_color   = "grey80",
    legend       = FALSE
) {
  # open and size window
  open3d()
  par3d(windowRect = c(50, 50, 1000, 1000))
  
  # position the camera via polar angles instead of rgl.camera()
  phi_deg <- atan2(grid_size * 0.5, grid_size * 1.5) * 180 / pi
  view3d(theta = 0, phi = phi_deg, fov = 60)
  
  # keep aspect ratio and defer redraw until all drawing is done
  aspect3d(1, 1, 1)
  par3d(skipRedraw = TRUE)
  
  # draw grid
  xlim <- c(0, grid_size); zlim <- c(0, grid_size)
  xs   <- seq(xlim[1], xlim[2], by = 1)
  zs   <- seq(zlim[1], zlim[2], by = 1)
  for (z in zs) lines3d(x = xlim, y = 0,   z = c(z,z), color = grid_color)
  for (x in xs) lines3d(x = x,    y = c(0,0), z = zlim,   color = grid_color)
  lines3d(x = xlim, y = 0, z = 0,   col = "red",  lwd = 4)
  lines3d(x = 0,    y = 0, z = zlim, col = "blue", lwd = 4)
  
  # draw edges
  if (all(c("color","width") %in% names(edges))) {
    styles <- distinct(edges, color, width)
    for (i in seq_len(nrow(styles))) {
      st  <- styles[i, ]
      sub <- filter(edges, color == st$color & width == st$width)
      coords <- as.numeric(t(as.matrix(sub[, c("x","y","z","x_end","y_end","z_end")])))
      segments3d(coords, color = st$color, lwd = st$width, alpha = 0.6)
    }
  }
  
  # draw nodes
  node_cols <- if (!is.null(feature_cols)) {
    feature_cols[ as.character(nodes$item) ]
  } else {
    rep("black", nrow(nodes))
  }
  spheres3d(
    x      = nodes$x,
    y      = nodes$y,
    z      = nodes$z,
    radius = nodes$radius,
    color  = node_cols
  )
  
  # flush drawing
  par3d(skipRedraw = FALSE)
}