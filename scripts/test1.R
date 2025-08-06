library(niarules)
library(dplyr)

# this loads a precomputed ruleset (one that has longer rules)
rules_csv <- file.path("scripts", "abalone_rules1.csv")
rules_df <- read.csv(rules_csv, stringsAsFactors = FALSE)

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

# how many radial‐plots (one per unique consequent) we need
n_plots   <- length(unique(wdf$rhs))
grid_size <- ceiling(sqrt(n_plots))

res <- buildRadialPlots(wdf, grid_size)
render_radial_rgl(res$nodes, res$edges, grid_size, NULL, "lightblue", FALSE)

