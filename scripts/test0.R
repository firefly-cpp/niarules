read_rules_csv_for_parse <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  cn  <- tolower(names(raw))
  names(raw) <- cn
  
  # required columns in this CSV
  need <- c("rules", "support", "confidence", "lift")
  miss <- setdiff(need, cn)
  if (length(miss)) {
    stop("CSV is missing columns: ", paste(miss, collapse = ", "))
  }
  
  # Split "{...} => {...}" into LHS/RHS, trim both sides
  rs <- as.character(raw$rules)
  parts <- strsplit(rs, "=>", fixed = TRUE)
  
  lhs <- vapply(parts, function(x) trimws(if (length(x) >= 1) x[1] else ""), character(1))
  rhs <- vapply(parts, function(x) trimws(if (length(x) >= 2) x[2] else ""), character(1))
  
  # Vectorized "strip one layer of outer { }" (if present)
  strip_braces_one <- function(x) {
    s <- trimws(as.character(x))
    n <- nchar(s)
    has <- n >= 2L & substr(s, 1L, 1L) == "{" & substr(s, n, n) == "}"
    s[has] <- substr(s[has], 2L, n[has] - 1L)
    s
  }
  lhs <- strip_braces_one(lhs)
  rhs <- strip_braces_one(rhs)
  
  data.frame(
    Antecedent  = lhs,
    Consequence = rhs,                 # keep combined RHS; our parser splits it
    Support     = as.numeric(raw$support),
    Confidence  = as.numeric(raw$confidence),
    Fitness     = as.numeric(raw$lift),  # we treat this as "lift"
    stringsAsFactors = FALSE
  )
}

rules_path <- system.file("extdata", "abalone_rules.csv", package = "niarules", mustWork = TRUE)
df <- read_rules_csv_for_parse(rules_path)
parsed  <- parse_rules(df)
layout  <- build_coral_plots(parsed, lhs_sort_metric = "confidence")

render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  # styling choices (all in R now)
  edge_metric          = "confidence",                  # or "support"/"lift"
  edge_width_range     = c(1.5, 6),
  edge_width_transform = "sqrt",
  edge_gradient        = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
  node_color_by        = "type",                        # or "item"/"none"
  node_colors          = NULL,                          # named overrides if you want
  palette_hcl_c        = 80,
  palette_hcl_l        = 50
)