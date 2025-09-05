
library(dplyr)
library(readr)

# 0) Load your harvested (full-metrics) rules
df <- read_csv("harvest_rules_full.csv", show_col_types = FALSE)

# 1) Coalesce metrics to the full-data versions (fall back if missing)
df <- df %>%
  mutate(
    support    = ifelse(!is.na(Support_full),    Support_full,    Support),
    confidence = ifelse(!is.na(Confidence_full), Confidence_full, Confidence),
    lift       = ifelse(!is.na(Lift_full),       Lift_full,       Fitness)
  )

# 2) Build a de-dup key that ignores LHS order and RHS text; key = (LHS set, ring v)
split_outside_brackets <- function(s) {
  if (!nzchar(s)) return(character(0))
  res <- character(0); buf <- ""; depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch %in% c("[","(")) depth <- depth + 1L
    else if (ch %in% c("]",")")) depth <- max(0L, depth - 1L)
    if (depth == 0L && ch %in% c(",", "&")) {
      part <- trimws(buf); if (nzchar(part)) res <- c(res, part); buf <- ""
    } else buf <- paste0(buf, ch)
  }
  part <- trimws(buf); if (nzchar(part)) res <- c(res, part)
  res
}

lhs_key <- function(lhs_str) {
  lhs_core  <- sub("^\\{(.*)\\}$", "\\1", as.character(lhs_str))
  items <- if (nzchar(lhs_core)) split_outside_brackets(lhs_core) else character(0)
  paste(sort(items[nzchar(items)]), collapse = "|")
}

df <- df %>%
  mutate(lhs_key = vapply(Antecedent, lhs_key, character(1)),
         rhs_label = paste0("Rings (", v, ")")) %>%   # explicit label for your RHS filter
  group_by(v, lhs_key) %>%
  # keep the "best" duplicate (pick your ordering; here: Confidence_full > Confidence > Fitness)
  slice_max(order_by = coalesce(Confidence_full, Confidence, Fitness, 0), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-lhs_key)

target_ring <- 11                # <-- change this for each coral you want
top_n_rules <- NA_integer_       # e.g., 30 to cap; or NA to keep all

df_ring <- df %>% filter(v == target_ring)
if (!is.na(top_n_rules)) {
  df_ring <- df_ring %>%
    arrange(desc(confidence), desc(lift), desc(support)) %>%
    slice_head(n = top_n_rules)
}
df_ring$Antecedent <- gsub("^\\{\\s*|\\s*\\}$", "", df_ring$Antecedent)
df_ring$Support    <- ifelse(!is.na(df_ring$Support_full),    df_ring$Support_full,    df_ring$Support)
df_ring$Confidence <- ifelse(!is.na(df_ring$Confidence_full), df_ring$Confidence_full, df_ring$Confidence)
df_ring$Fitness    <- ifelse(!is.na(df_ring$Lift_full),       df_ring$Lift_full,       df_ring$Fitness)
df_ring

parsed  <- niarules::parse_rules(df_ring)
parsed

layout <- niarules::build_coral_plots(parsed, lhs_sort_metric = "confidence")

dom <- niarules::metric_domains(layout$edges)

niarules::render_coral_rgl(
  layout$nodes, layout$edges, layout$grid_size,
  legend      = TRUE,
  legend_style= "grouped",
  legend_pos  = "inside_right",
  legend_cex  = 1.15,
  
  label_mode  = "interval_short",
  max_labels  = 0,
  theme       = "flat",
  
  node_color_by     = "type",
  node_gradient     = c("#aa7bb6", "#aaf7f7", "#aa191c"),
  node_gradient_map = "hash",
  
  edge_width_metric    = "support",    edge_width_transform = "sqrt",
  edge_width_range     = c(1, 6),      edge_width_domain  = dom$support,
  
  edge_color_metric    = "lift",       edge_color_transform = "sqrt",
  edge_gradient        = c("#2c7bb6", "#f7f7f7", "#d7191c"),
  edge_color_domain    = dom$lift,
  
  edge_alpha_metric    = "confidence", edge_alpha_range = c(0.2, 1),
  edge_alpha_domain    = dom$confidence,
  
  y_scale = 0.18, jitter_sd = 0.02, jitter_mode = "random", jitter_seed = 1248
)