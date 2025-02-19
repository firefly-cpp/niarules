#' Print Numerical Association Rules
#'
#' This function prints association rules including antecedent, consequence,
#' support, confidence, and fitness. For time series datasets, it also includes
#' the start and end timestamps instead of indices.
#'
#' @param rules A list containing association rules.
#' @param is_time_series A boolean flag indicating if time series information should be included.
#' @param timestamps A vector of timestamps corresponding to the time series data.
#'
#' @return Prints the association rules.
#' @export
print_association_rules <- function(rules, is_time_series = FALSE, timestamps = NULL) {
  # Filter out invalid rules
  rules <- Filter(function(x) is.list(x) && !is.null(x$fitness), rules)

  if (length(rules) == 0) {
    message("No valid rules to display.")
    return()
  }

  # Sort rules by fitness
  rules <- rules[order(sapply(rules, function(x) -x$fitness))]

  message("Association Rules:")

  for (i in seq_along(rules)) {
    rule <- rules[[i]]

    message("\nRule", i, ":")

    message("Antecedent:", toString(format_rule_parts(rule$antecedent)))
    message("Consequence:", toString(format_rule_parts(rule$consequent)))

    message("Support:", rule$support)
    message("Confidence:", rule$confidence)
    message("Fitness:", rule$fitness)

    # Replace indexes with timestamps if time series
    if (is_time_series && !is.null(rule$time_series_range)) {
      start_index <- rule$time_series_range$start
      end_index <- rule$time_series_range$end

      # Map indexes to actual timestamps if timestamps vector is valid
      if (!is.null(timestamps) && start_index <= length(timestamps) && end_index <= length(timestamps)) {
        start_timestamp <- as.character(timestamps[start_index])
        end_timestamp <- as.character(timestamps[end_index])
        message("Start Time:", start_timestamp)
        message("End Time:", end_timestamp)
      } else {
        message("Start Time Index:", start_index)
        message("End Time Index:", end_index)
      }
    }

    message("----------------------")
  }
}

#' Format Rule Parts
#'
#' This function formats the parts of an association rule into a string.
#'
#' @param parts A list containing parts of an association rule.
#'
#' @return A formatted string representing the rule parts.
format_rule_parts <- function(parts) {
  sapply(parts, function(part) {
    if (part$type == "numerical") {
      paste0(part$name, " (", part$border1, ",", part$border2, ")")
    } else {
      paste0(part$name, " (", part$value, ")")
    }
  })
}

#' Write Association Rules to CSV file
#'
#' This function writes association rules to a CSV file. For time series datasets,
#' it also includes start and end timestamps instead of indices.
#'
#' @param rules A list of association rules.
#' @param file_path The file path for the CSV output.
#' @param is_time_series A boolean flag indicating if time series information should be included.
#' @param timestamps A vector of timestamps corresponding to the time series data.
#'
#' @export
#'
#' @return No explicit return value. The function writes association rules to a CSV file.
#'
write_association_rules_to_csv <- function(rules, file_path, is_time_series = FALSE, timestamps = NULL) {
  # Extracting relevant information from the nested structure
  rules_data <- lapply(rules, function(rule) {
    antecedent_str <- toString(format_rule_parts(rule$antecedent))
    consequence_str <- toString(format_rule_parts(rule$consequent))

    if (is_time_series && !is.null(rule$time_series_range) && !is.null(timestamps)) {
      start_index <- rule$time_series_range$start
      end_index <- rule$time_series_range$end

      start_timestamp <- ifelse(start_index <= length(timestamps), as.character(timestamps[start_index]), NA)
      end_timestamp <- ifelse(end_index <= length(timestamps), as.character(timestamps[end_index]), NA)

      c(Antecedent = antecedent_str,
        Consequence = consequence_str,
        Support = rule$support,
        Confidence = rule$confidence,
        Fitness = rule$fitness,
        StartTime = start_timestamp,
        EndTime = end_timestamp)
    } else {
      c(Antecedent = antecedent_str,
        Consequence = consequence_str,
        Support = rule$support,
        Confidence = rule$confidence,
        Fitness = rule$fitness)
    }
  })

  # Convert the list to a data frame
  rules_df <- data.frame(do.call(rbind, rules_data), stringsAsFactors = FALSE)

  # Write to CSV
  write.csv(rules_df, file = file_path, row.names = FALSE)

  message("Association rules successfully written to:", file_path)
}
