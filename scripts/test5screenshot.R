library(readr)
library(arules)
library(niarules)

set.seed(1248)

# --- data ------------------------------------------------------------------
path <- system.file("extdata", "Abalone.csv", package = "niarules")
abalone <- readr::read_csv(path, show_col_types = FALSE)
names(abalone) <- c("Sex","Length","Diameter","Height",
                    "WholeWeight","ShuckedWeight","VisceraWeight",
                    "ShellWeight","Rings")
abalone$Sex   <- factor(abalone$Sex)
abalone$Rings <- factor(abalone$Rings)

# discretize: equal-frequency bins (unsupervised, defensible)
disc <- arules::discretizeDF(abalone, default = list(method = "frequency", breaks = 6))
tr <- as(disc, "transactions")
rhs_labs <- grep("^Rings=", arules::itemLabels(tr), value = TRUE)

# --- mine ------------------------------------------------------------------
rules <- arules::apriori(
  tr,
  parameter = list(supp = 0.002, conf = 0.5, minlen = 2, maxlen = 8),
  appearance = list(rhs = rhs_labs, default = "lhs"),
  control = list(verbose = FALSE)
)
stopifnot(length(rules) > 0L)

# Optional pruning (usually leave OFF for fairness in the paper)
# rules <- rules[!arules::is.redundant(rules)]

# --- deterministic ranking + per-RHS cap ----------------------------------
q0  <- arules::quality(rules)
lhs_txt0 <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::lhs(rules)))
rhs_txt0 <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules)))
ord <- order(-q0$lift, -q0$confidence, -q0$support, rhs_txt0, lhs_txt0, na.last = NA)

rules_ord <- rules[ord]
rhs_ord   <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules_ord)))
pos_by_rhs <- split(seq_along(rules_ord), rhs_ord)

k <- 250
keep_pos <- unlist(lapply(pos_by_rhs, head, k), use.names = FALSE)
rules_bal <- rules_ord[keep_pos]
stopifnot(length(rules_bal) > 0L)

# --- PICK RHS (uncomment ONE of the following) -----------------------------

# 1) Most frequent RHS (simple & fair)
#rhs_choice <- names(sort(table(rhs_ord), decreasing = TRUE))[1]

# 2) Median RHS by rule count (avoid extremes)
# tab <- sort(table(rhs_ord))
# rhs_choice <- names(tab)[ceiling(length(tab)/2)]

# 3) Median "branchiness" (objective but a tad fancier)
lhs_len <- arules::size(arules::lhs(rules_bal))
branch <- tapply(lhs_len, rhs_ord, function(v) c(n=length(v), uniq_len=length(unique(v)), mean_len=mean(v)))
B <- transform(as.data.frame(do.call(rbind, branch)), rhs = rownames(do.call(rbind, branch)))
# optional readability filter:
B <- subset(B, n >= 80 & uniq_len >= 3)
B <- B[order(B$mean_len, B$rhs), ]
rhs_choice <- B$rhs[ceiling(nrow(B)/2)]

# --- safety (make sure you picked one) -------------------------------------
if (!exists("rhs_choice")) stop("Uncomment ONE rhs_choice strategy.")
message("RHS chosen: ", rhs_choice)

# subset to the chosen RHS
rhs_ord2 <- sub("^\\{(.*)\\}$","\\1", arules::labels(arules::rhs(rules_bal)))
rules_one <- rules_bal[rhs_ord2 == rhs_choice]
stopifnot(length(rules_one) > 0L)

# --- parse (no CSV hop) ----------------------------------------------------
q <- arules::quality(rules_one)  # recompute for the final subset
df <- data.frame(
  Antecedent  = sub("^\\{(.*)\\}$", "\\1", labels(lhs(rules_one))),
  Consequence = sub("^\\{(.*)\\}$", "\\1", labels(rhs(rules_one))),
  Support     = as.numeric(q$support),
  Confidence  = as.numeric(q$confidence),
  Fitness     = as.numeric(q$lift),   # parser treats Fitness as lift
  check.names = FALSE
)
parsed <- niarules::parse_rules(df)

# build layout (single coral)
layout <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")

# optional: global domains (fair scaling within the figure)
dom <- niarules::metric_domains(layout$edges)

# --- render ----------------------------------------------------------------
niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  
  label_mode = "interval_short",
  label_color = "black",
  label_cex = 1.0,
  label_offset = 20.0,
  max_labels = 0,
  theme = "flat",
  
  legend = TRUE,
  legend_style = "grouped",
  legend_pos = "inside_right",
  legend_cex = 1.15,
  
  node_color_by     = "type",
  node_gradient     = c("red", "orange", "yellow", "green", "cyan", "blue", "magenta"),
  node_gradient_map = "even",
  
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1, 6),      #edge_width_domain  = dom$support,
  
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6", "#f7f7f7", "#d7191c"),
  #edge_color_domain    = dom$lift,
  
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.2, 1),
  #edge_alpha_domain    = dom$confidence,
  
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)

# rgl::view3d(theta = -32, phi = 16, fov = 22, zoom = 0.95)
# rgl::par3d(antialias = 8)
rgl::rgl.snapshot("s1.png", fmt = "png", top = TRUE)