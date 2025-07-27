library(niarules)
library(dplyr)
library(tidyr)

# make sure your working dir is the package root:
stopifnot(basename(getwd()) == "niarules")

# build the path to the CSV
rules_csv <- file.path("scripts", "abalone_rules1.csv")

rules_df <- read.csv(rules_csv, stringsAsFactors = FALSE)

#unfortunately, this needs another function to get the "wide" dataset
get_rules_wide <- function(rules_df) {
  rules_df %>%
    # ensure we have the right types
    mutate(
      rule_id            = as.integer(rule_id),
      support            = as.numeric(support),
      confidence         = as.numeric(confidence),
      lift               = as.numeric(lift),
      antecedent_length  = as.integer(antecedent_length),
      rhs                = as.character(rhs)
    ) %>%
    # keep only the columns the C++ side will use,
    # in the exact order it expects them:
    select(
      rule_id,
      support,
      confidence,
      lift,
      rhs,
      antecedent_length,
      starts_with("lhs_")
    )
}

wdf <- get_rules_wide(rules_df)

# how many little radial‐plots (one per unique consequent) we need
n_plots   <- length(unique(wdf$rhs))
# pack them into as square a grid as possible
grid_size <- ceiling(sqrt(n_plots))

res <- wrapped_buildRadialPlots(wdf, grid_size)
head(res$edges)
head(res$nodes)

#render

render_radial_rgl <- function(
    nodes,
    edges,
    grid_size,
    feature_cols = NULL,
    grid_color = "grey80",
    legend = FALSE
)
  {
  library(rgl)
  library(rglplus)
  
  open3d()
  par3d(windowRect = c(50, 50, 1000, 1000))
  camera_position <- c(0, grid_size * 0.5, grid_size * 1.5)
  camera_center   <- c(0, 0, 0)
  camera_forward  <- camera_center - camera_position
  rgl.camera(
    camera_position,
    camera_center,
    c(0,1,0),
    fov = 60
  )
  aspect3d(1, 1, 1)
  par3d(skipRedraw = TRUE)
  
  # draw grid
  xlim <- c(0, grid_size); zlim <- c(0, grid_size)
  xs <- seq(xlim[1], xlim[2], by = 1)
  zs <- seq(zlim[1], zlim[2], by = 1)
  for (z in zs) lines3d(x = xlim, y = c(0,0), z = c(z,z), color = grid_color)
  for (x in xs) lines3d(x = c(x,x), y = c(0,0), z = zlim, color = grid_color)
  lines3d(x = xlim, y = c(0,0), z = c(0,0), col = "red", lwd = 4)
  lines3d(x = c(0,0), y = c(0,0), z = zlim, col = "blue", lwd = 4)
  
  #draw edges
  if (all(c("color","width") %in% names(edges)))
  {
    styles <- distinct(edges, color, width)
    for (i in seq_len(nrow(styles)))
    {
      st  <- styles[i,]
      sub <- filter(edges, color == st$color & width == st$width)
      coords <- as.numeric(t(as.matrix(sub[, c("x","y","z","x_end","y_end","z_end")])))
      segments3d(coords, color = st$color, lwd = st$width, alpha = 0.6)
    }
  }
  
  # draw nodes
  if (!is.null(feature_cols)) {
    node_cols <- feature_cols[as.character(nodes$item)]
  } else {
    node_cols <- rep("black", nrow(nodes))
  }
  spheres3d(
    x      = nodes$x,
    y      = nodes$y,
    z      = nodes$z,
    radius = nodes$radius,
    color  = node_cols
  )
  
  par3d(skipRedraw = FALSE)
}

#range(res$nodes$x)
#range(res$nodes$z)

render_radial_rgl(res$nodes, res$edges, grid_size, NULL, "lightblue", FALSE)

