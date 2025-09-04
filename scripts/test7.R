library(readr)
library(arules)
library(arulesCBA)

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

abalone <- read_csv("abalone_data.csv",
                    col_names = c("Sex","Length","Diameter","Height",
                                  "WholeWeight","ShuckedWeight","VisceraWeight",
                                  "ShellWeight","Rings"),
                    show_col_types = FALSE
)
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# unsupervised discretization (tends to create more overlapping combos)
disc <- arules::discretizeDF(
  abalone, default = list(method = "cluster", breaks = 7)  # also try breaks=8
)

tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", itemLabels(tr), value = TRUE)

rules <- apriori(
  tr,
  parameter = list(
    supp   = 0.001,
    conf   = 0.35,
    minlen = 3,
    maxlen = 10
  ),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)

stopifnot(length(rules) > 0)
q <- quality(rules)

rhs_str <- labels(rhs(rules))
rhs_counts <- sort(table(rhs_str), decreasing = TRUE)
requested_rhs <- names(rhs_counts)[1]
r <- rules[rhs_str == requested_rhs]
q <- quality(r)

lhs_chr <- labels(lhs(r))
lhs_chr <- sub("^\\{(.*)\\}$", "\\1", lhs_chr)
tok <- strsplit(lhs_chr, "\\s*,\\s*")

first  <- vapply(tok, `[`, "", 1L)
second <- vapply(tok, function(x) if (length(x)>=2) x[2] else NA_character_, "")

# quotas: top per (first), within that per (first,second), ranked by lift/conf
k_first  <- 12   # families by first item
k_second <- 8    # branches per family
k_leaf   <- 6    # leaves per branch

ord_global <- order(q$lift, q$confidence, q$support, decreasing = TRUE, na.last = NA)
idx_all <- integer()

for (f in head(names(sort(table(first), decreasing = TRUE)), k_first)) {
  ix_f <- which(first == f)
  # rank within this first-item family
  ix_f <- ix_f[order(q$lift[ix_f], q$confidence[ix_f], decreasing = TRUE, na.last = NA)]
  # split by second item to create sub-branches
  sec_vals <- head(names(sort(table(second[ix_f]), decreasing = TRUE)), k_second)
  for (s in sec_vals) {
    j <- ix_f[which(second[ix_f] == s)]
    if (length(j)) {
      j <- j[order(q$lift[j], q$confidence[j], decreasing = TRUE, na.last = NA)]
      idx_all <- c(idx_all, head(j, k_leaf))
    }
  }
}
idx_all <- unique(idx_all)
r_balanced <- r[idx_all]
length(r_balanced)

rite_rules_csv <- function(rules, file) {
  lhs_chr <- labels(lhs(rules))
  rhs_chr <- labels(rhs(rules))
  q <- quality(rules)
  out <- data.frame(
    Antecedent  = lhs_chr,
    Consequence = rhs_chr,
    Support     = as.numeric(q$support),
    Confidence  = as.numeric(q$confidence),
    Fitness     = as.numeric(q$lift),
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, file, row.names = FALSE)
}

write_rules_csv(r_balanced, "inst/extdata/abalone_rules.csv")
df      <- utils::read.csv("inst/extdata/abalone_rules.csv", check.names = FALSE)
df$Antecedent  <- sub("^\\{(.*)\\}$", "\\1", df$Antecedent)   # ensure LHS braces removed
df$Consequence <- sub("^\\{(.*)\\}$", "\\1", df$Consequence)  # safe on RHS too

parsed  <- niarules::parse_rules(df)
layout  <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  label_mode   = "item", max_labels = 0, label_color = "grey20",
  
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1.2, 7.0),  edge_width_domain = dom$support,
  
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6","#f7f7f7","#d7191c"),
  edge_color_domain    = dom$lift,
  
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.15, 1),
  edge_alpha_transform = "linear",     edge_alpha_domain = dom$confidence,
  
  node_color_by        = "item",
  y_scale = 0.18, jitter_sd = 0.012, jitter_mode = "random", jitter_seed = 1248,
  radial_expand = 1.35, radial_gamma = 1.45
  #node_radius_scale = 0.97, node_alpha_by_depth = TRUE
)

rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
rgl::par3d(antialias = 8)
rgl::rgl.snapshot("test7.png", fmt = "png", top = TRUE)