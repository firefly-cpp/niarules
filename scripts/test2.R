library(niarules)
library(dplyr)
library(tidyr)

# make sure your working dir is the package root:
stopifnot(basename(getwd()) == "niarules")

# build the path to the CSV
rules_csv <- file.path("scripts", "abalone_rules2.csv")

rules_df <- read.csv(rules_csv, stringsAsFactors = FALSE)

get_rules_wide_niarules <- function(rules_df)
{
  rules_df %>%
    # add an ID and normalize column names
    mutate(
      rule_id = row_number(),
      lhs     = gsub("^\\{|\\}$", "", Antecedent),
      rhs     = gsub("^\\{|\\}$", "", Consequence)
    ) %>%
    # one row per LHS item
    separate_rows(lhs, sep = ",") %>%
    # trim whitespace, and rename support/confidence/lift
    mutate(
      from = trimws(lhs),
      to   = trimws(rhs)
    ) %>%
    select(from, to, rule_id, 
           support    = Support, 
           confidence = Confidence, 
           lift       = Fitness) %>%
    # number the items per rule
    group_by(rule_id) %>%
    arrange(rule_id, from) %>%
    mutate(
      item_idx          = row_number(),
      antecedent_length = n()
    ) %>%
    ungroup() %>%
    # pivot to wide: lhs_1, lhs_2, …
    pivot_wider(
      id_cols      = c(rule_id, support, confidence, lift, to, antecedent_length),
      names_from   = item_idx,
      values_from  = from,
      names_prefix = "lhs_"
    ) %>%
    rename(rhs = to)
}

data_raw <- niarules::read_dataset("inst/extdata/Abalone.csv")

features <- niarules::extract_feature_info(data_raw)
d <- niarules::problem_dimension(features, is_time_series = FALSE)

de <- niarules::differential_evolution(
  d        = d,
  np       = 30,
  f        = 0.5,
  cr       = 0.9,
  nfes     = 1000,
  features = features,
  data     = data_raw,
  is_time_series = FALSE
)

niarules::write_association_rules_to_csv(
  de$arules,
  file       = rules_csv,
  is_time_series = FALSE
)

# and build the "wide" dataframe
wdf <- get_rules_wide_niarules(rules_df)

########################################################################################
# our real stuff starts here

library(niarules)
#ls("package:niarules")

# how many little radial‐plots (one per unique consequent) we need
n_plots   <- length(unique(wdf$rhs))
# pack them into as square a grid as possible
grid_size <- ceiling(sqrt(n_plots))

res <- wrapped_buildRadialPlots(wdf, grid_size)
#head(res$edges)
#head(res$nodes)

#render function, currently lacking interaction
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

render_radial_rgl(res$nodes, res$edges, grid_size, NULL, "lightblue", FALSE)

