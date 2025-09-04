set.seed(1248)

write_rules_csv <- function(rules, file) {
  stopifnot(inherits(rules, "rules"))
  
  q <- arules::quality(rules)
  
  df <- data.frame(
    Antecedent  = arules::labels(arules::lhs(rules)),
    Consequence = arules::labels(arules::rhs(rules)),
    Support     = as.numeric(q$support),
    Confidence  = as.numeric(q$confidence),
    Fitness     = as.numeric(q$lift),  # your code treats this as lift
    stringsAsFactors = FALSE
  )
  
  if ("count" %in% names(q)) df$Count <- as.integer(q$count)
  
  utils::write.csv(df, file = file, row.names = FALSE, quote = TRUE)
  invisible(file)
}

raw_csv <- "abalone_data.csv"
if (!file.exists(raw_csv)) {
  readr::write_csv(
    readr::read_csv(
      "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
      col_names = c("Sex","Length","Diameter","Height","WholeWeight",
                    "ShuckedWeight","VisceraWeight","ShellWeight","Rings"),
      progress = FALSE
    ),
    raw_csv
  )
}
abalone <- readr::read_csv(raw_csv, show_col_types = FALSE)
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# discretize: more balanced bins than k-means
disc <- arules::discretizeDF(
  abalone,
  default = list(method = "frequency", breaks = 6)  # ~equal-frequency bins
)

tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", arules::itemLabels(tr), value = TRUE)

# mine with stricter but still permissive thresholds
rules <- arules::apriori(
  tr,
  parameter = list(
    supp   = 0.002,
    conf   = 0.5,
    minlen = 2,
    maxlen = 8
  ),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)

# remove redundant rules
rules <- rules[!arules::is.redundant(rules)]

# rank by multiple measures: lift desc, confidence desc, support desc
q <- arules::quality(rules)
stopifnot(all(c("lift","confidence","support") %in% names(q)))

ord <- do.call(order, list(-q$lift, -q$confidence, -q$support, na.last = NA))
rules <- rules[ord]

# optionally cap top-k per RHS to avoid a few RHS dominating the coral:
k <- 250
rhs_lab <- arules::labels(arules::rhs(rules))
# preserve the global ranking within each RHS
split_ord <- split(ord, rhs_lab[ord])
keep <- unlist(lapply(split_ord, function(ix) head(ix, k)), use.names = FALSE)
rules <- rules[keep]

write_rules_csv(rules, "inst/extdata/abalone_rules.csv")

rules_path <- system.file("extdata", "abalone_rules.csv", package = "niarules", mustWork = TRUE)
df <- utils::read.csv(rules_path, stringsAsFactors = FALSE, check.names = FALSE)

strip_braces_one <- function(x) {
  s <- trimws(as.character(x))
  n <- nchar(s)
  has <- n >= 2L & substring(s, 1L, 1L) == "{" & substring(s, n, n) == "}"
  s[has] <- substring(s[has], 2L, n[has] - 1L)
  s
}

df$Antecedent  <- strip_braces_one(df$Antecedent)
df$Consequence <- strip_braces_one(df$Consequence)

parsed <- niarules::parse_rules(df)
table(parsed$rules$antecedent_length)  # should now show >1 lengths

layout  <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")
#layout

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  label_mode = "interval_short", max_labels = 0,
  theme = "flat",
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1, 6),
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6", "#f7f7f7", "#d7191c"),
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.2, 1),
  node_color_by        = "item",
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)

rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
rgl::par3d(antialias = 8)
rgl::rgl.snapshot("test5.png", fmt = "png", top = TRUE)