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

  cat("Association Rules:\n")

  for (i in seq_along(rules)) {
    rule <- rules[[i]]

    cat("\nRule", i, ":\n")

    cat("Antecedent:")
    print_rule_parts(rule$antecedent)

    cat("\nConsequence:")
    print_rule_parts(rule$consequence)

    cat("\nSupport:", rule$support, "\n")
    cat("Confidence:", rule$confidence, "\n")
    cat("Fitness:", rule$fitness, "\n")

    cat("----------------------\n")
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
      cat(part$name, "(", part$border1, ",", part$border2, ")")
    } else {
      cat(part$name, "(", part$value, ")")
    }

    if (i < len) {
      cat(" & ")
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
#' @examples
#' arules <- list(list(
#'   list(
#'     antecedent = list(
#'       list(name = "Diameter", type = "numerical", border1 = 0, border2 = 1, value = "EMPTY")
#'     ),
#'     consequence = list(
#'       list(name = "Height", type = "numerical", border1 = 0, border2 = 1, value = "EMPTY")
#'     ),
#'     support = 0.9997606,
#'     confidence = 0.9997606,
#'     fitness = 1.999521
#'   )
#' ))
#' write_association_rules_to_csv(arules, "association_rules_output.csv")
#'
#' @export
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

  cat("Numerical Association rules successfully written to:", file_path, "\n")
}

