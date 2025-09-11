#' Available themes (experimental)
#' @description `r lifecycle::badge('experimental')`
#' @return character vector of theme names
#' @export
coral_list_themes <- function() c("default", "studio", "flat", "dark", "none")

#' Get a theme object (experimental)
#' @description `r lifecycle::badge('experimental')`
#' @param name one of coral_list_themes()
#' @param overrides named list to tweak values
#' @return an object of class 'coral_theme'
#' @export
coral_get_theme <- function(name = c("default", "studio", "flat", "dark", "none"),
                            overrides = NULL) {
  name <- match.arg(name)
  base <- switch(
    name,
    "default" = list(
      background = "white",
      grid_color = "grey92",
      materials = list(
        nodes  = list(specular = "grey40", shininess = 20, ambient = "grey10", emission = "black"),
        edges  = list(specular = "grey30", shininess = 15, ambient = "grey10", emission = "black"),
        labels = list(emission = "black", specular = "black", shininess = 0)
      ),
      lights = list(
        list(theta = 45,  phi = 30,  viewpoint = FALSE, ambient = "#444444", diffuse = "white",  specular = "white"),
        list(theta = 225, phi = 10,  viewpoint = FALSE, ambient = "#222222", diffuse = "white",  specular = "white")
      )
    ),
    "studio" = list(
      background = "white",
      grid_color = "grey95",
      materials = list(
        nodes  = list(specular = "white", shininess = 60, ambient = "grey20", emission = "black"),
        edges  = list(specular = "white", shininess = 40, ambient = "grey15", emission = "black"),
        labels = list(emission = "black", specular = "black", shininess = 0)
      ),
      lights = list(
        list(theta = 45,  phi = 35, viewpoint = FALSE, diffuse = "white", specular = "white"),
        list(theta = -30, phi = 15, viewpoint = FALSE, diffuse = "white", specular = "white"),
        list(theta = 180, phi = -20, viewpoint = FALSE, ambient = "#333333", diffuse = "#eeeeee")
      )
    ),
    "flat" = list(
      background = "white",
      grid_color = "grey90",
      materials = list(
        nodes  = list(specular = "black", shininess = 0, ambient = "grey40", emission = "black"),
        edges  = list(specular = "black", shininess = 0, ambient = "grey40", emission = "black"),
        labels = list(emission = "black", specular = "black", shininess = 0)
      ),
      lights = list( # mostly ambient feel
        list(theta = 0, phi = 0, viewpoint = TRUE, ambient = "#777777", diffuse = "#dddddd")
      )
    ),
    "dark" = list(
      background = "#0b0b10",
      grid_color = "#1b1b25",
      materials = list(
        nodes  = list(specular = "white", shininess = 50, ambient = "grey20", emission = "black"),
        edges  = list(specular = "white", shininess = 30, ambient = "grey20", emission = "black"),
        labels = list(emission = "white", specular = "black", shininess = 0)
      ),
      lights = list(
        list(theta = 45,  phi = 35, viewpoint = FALSE, diffuse = "white", specular = "white"),
        list(theta = 220, phi = 5,  viewpoint = FALSE, diffuse = "#cccccc", specular = "#cccccc"),
        list(theta = 0,   phi = 90, viewpoint = TRUE,  ambient = "#333333", diffuse = "#bbbbbb")
      )
    ),
    "none" = list(
      background = "white",
      grid_color = "grey92",
      materials = list(
        nodes  = list(specular = "black", shininess = 0, ambient = "grey30", emission = "black"),
        edges  = list(specular = "black", shininess = 0, ambient = "grey30", emission = "black"),
        labels = list(emission = "black", specular = "black", shininess = 0)
      ),
      lights = NULL
    )
  )
  
  # merge overrides (shallow)
  if (!is.null(overrides)) {
    base <- utils::modifyList(base, overrides, keep.null = TRUE)
  }
  base
}

#' @keywords internal
#' @noRd
coral_apply_theme <- function(theme) {
  # background
  if (!is.null(theme$background)) rgl::bg3d(color = theme$background)
  
  # lights (replace all)
  rgl::clear3d(type = "lights")
  if (!is.null(theme$lights) && length(theme$lights)) {
    for (L in theme$lights) do.call(rgl::light3d, L)
  }
  
  # set a sane global material default; per-primitive overrides happen before drawing
  if (!is.null(theme$materials) && length(theme$materials)) {
    # global baseline (very mild)
    do.call(rgl::material3d, c(list(front = "fill"),
                               theme$materials$global %||% list(ambient = "grey15")))
  }
  
  invisible(theme)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
