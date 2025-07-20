library(ggplot2)
library(gridExtra)

#' Create a Rectangle Data Frame
#'
#' @param x Numeric, x-coordinate of the lower-left corner.
#' @param y Numeric, y-coordinate of the lower-left corner.
#' @param w Numeric, width of the rectangle.
#' @param h Numeric, height of the rectangle.
#' @return A data frame with x and y coordinates defining the rectangle.
draw_rectangle <- function(x, y, w, h) {
  data.frame(
    x = c(x, x + w, x + w, x),
    y = c(y, y, y + h, y + h),
    group = factor(1)
  )
}

#' Visualize a Rule on a Dataset
#'
#' @param rule List containing antecedent and consequent attributes.
#' @param dataset Data frame containing the transaction data.
#' @param path Character, file path to save the visualization (optional).
#' @param allfeatures Logical, whether to visualize all features.
#' @param antecedent Logical, whether to visualize antecedent attributes.
#' @param consequent Logical, whether to visualize consequent attributes.
#' @param timeseries Logical, whether the dataset is a time series.
#' @param intervalcolumn Character, name of the column representing intervals.
#' @param interval Integer, specific interval to visualize.
#' @return A combined plot displaying the rule visualization.
narmviz <- function(rule, dataset, path = NULL, allfeatures = FALSE, antecedent = TRUE, consequent = TRUE, timeseries = FALSE, intervalcolumn = "interval", interval = 0) {
  df <- dataset

  if (timeseries) {
    df <- df[df[[intervalcolumn]] == interval, ]
    df[[intervalcolumn]] <- NULL
  }

  plots <- list()

  # First add antecedent attributes with shaded area
  if (antecedent) {
    for (attr in rule$antecedent) {
      plots[[attr$name]] <- ggplot(df, aes(x = seq_along(.data[[attr$name]]), y = .data[[attr$name]])) +
        geom_rect(aes(xmin = 0, xmax = nrow(df), ymin = attr$min, ymax = attr$max), fill = "purple", alpha = 0.15) +
        geom_point(color = "#1f78b4", alpha = 0.7, size = 3, shape = 16) +
        ggtitle(attr$name) +
        theme_light() +
        theme(plot.title = element_text(size = 14), axis.text = element_text(size = 12))
    }
  }

  # Then add consequent attributes with shaded area
  if (consequent) {
    for (attr in rule$consequent) {
      plots[[attr$name]] <- ggplot(df, aes(x = seq_along(.data[[attr$name]]), y = .data[[attr$name]])) +
        geom_rect(aes(xmin = 0, xmax = nrow(df), ymin = attr$min, ymax = attr$max), fill = "green", alpha = 0.15) +
        geom_point(color = "#e31a1c", alpha = 0.7, size = 3, shape = 16) +
        ggtitle(attr$name) +
        theme_light() +
        theme(plot.title = element_text(size = 14), axis.text = element_text(size = 12))
    }
  }

  # Then add all remaining features
  if (allfeatures) {
    selected_attributes <- c(sapply(rule$antecedent, `[[`, "name"), sapply(rule$consequent, `[[`, "name"))
    remaining_features <- setdiff(names(df), selected_attributes)
    for (feature in remaining_features) {
      if (is.numeric(df[[feature]])) {
        plots[[feature]] <- ggplot(df, aes(x = seq_along(.data[[feature]]), y = .data[[feature]])) +
          geom_point(color = "#33a02c", alpha = 0.7, size = 3, shape = 16) +
          ggtitle(feature) +
          theme_light() +
          theme(plot.title = element_text(size = 14), axis.text = element_text(size = 12))
      }
    }
  }

  combined_plot <- do.call(grid.arrange, c(plots, ncol = 2))

  if (is.null(path)) {
    print(combined_plot)
  } else {
    ggsave(filename = path, plot = last_plot(), width = 7, height = 7, units = "in")
  }
}
