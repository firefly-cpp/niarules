#' Print Numerical Association Rules
#'
#' This function prints association rules including antecedent, consequence,
#' support, confidence, and fitness.
#'
#' @param rules A list containing association rules.
#'
#' @return Prints the association rules.
#'
#' @export
print_association_rules <- function(rules) {

  # sort rules by fitness

  rules <- rules[order(sapply(rules, function(x) -x$fitness))]

  message("Association Rules:")

  for (i in seq_along(rules)) {
    rule <- rules[[i]]

    message("\nRule", i, ":")

    message("Antecedent:")
    print_rule_parts(rule$antecedent)

    message("\nConsequence:")
    print_rule_parts(rule$consequence)

    message("\nSupport:", rule$support)
    message("Confidence:", rule$confidence)
    message("Fitness:", rule$fitness)

    message("----------------------")
  }
}

#' Print Rule Parts
#'
#' This function prints the parts of an association rule, including name, type,
#' border1, border2, and value.
#'
#' @param parts A list containing parts of an association rule.
#'
#' @return Prints the rule parts.
#'
print_rule_parts <- function(parts) {
  len <- length(parts)

  for (i in seq_along(parts)) {
    part <- parts[[i]]

    if (part$type != "categorical") {
      message(part$name, "(", part$border1, ",", part$border2, ")")
    } else {
      message(part$name, "(", part$value, ")")
    }

    if (i < len) {
      message(" & ")
    }
  }
}


#' Write Association Rules to CSV file
#'
#' This function writes association rules to a CSV file.
#'
#' @param rules A list of association rules.
#' @param file_path The file path for the CSV output.
#'
#' @export
#'
#' @return
#' No explicit return value. The function writes association rules to a CSV file
#' specified by the `file_path` parameter. A message is printed to the console
#' indicating the successful completion of the writing process.
#'
write_association_rules_to_csv <- function(rules, file_path) {
  # Extracting relevant information from the nested structure
  rules_data <- lapply(rules, function(rule) {
    antecedent_names <- sapply(rule$antecedent, function(part) part$name)
    consequence_names <- sapply(rule$consequence, function(part) part$name)

    c(Antecedent = toString(antecedent_names),
      Consequence = toString(consequence_names),
      Support = rule$support,
      Confidence = rule$confidence,
      Fitness = rule$fitness)
  })

  # Convert the list to a data frame
  rules_df <- data.frame(do.call(rbind, rules_data), stringsAsFactors = FALSE)

  # Write to CSV
  write.csv(rules_df, file = file_path, row.names = FALSE)

  message("Association rules successfully written to:", file_path)
}

