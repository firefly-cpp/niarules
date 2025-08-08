#' @title Build Data for Coral Plot Visualization from Association Rules
#'
#' @description
#' Converts an \code{arules}-like object into a structured list containing nodes, edges, and grid size
#' suitable for rendering with coral plot functions (e.g., \code{render_coral_rgl}).
#' Internally, this function serializes the rules to a temporary CSV file, processes them into
#' wide format, and calls \code{niarules::buildCoralPlots()} to compute layout geometry.
#'
#' @param arules An object containing association rules, typically of class \code{rules} from the
#'   \pkg{arules} or \pkg{niarules} package.
#'
#' @return A list with three components:
#' \describe{
#'   \item{\code{nodes}}{A data frame containing node positions, IDs, item names, and radii.}
#'   \item{\code{edges}}{A data frame of edges with coordinates, widths, and colors.}
#'   \item{\code{grid_size}}{An integer representing the layout grid dimension.}
#' }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Writes the input rules to a temporary CSV file using \code{niarules::write_association_rules_to_csv()}.
#'   \item Reads the CSV back in as a standard R data frame.
#'   \item Transforms the data into a wide format with one row per rule and columns \code{lhs_1}, \code{lhs_2}, etc.
#'   \item Computes the number of unique plots needed (based on consequents) and determines a square grid size.
#'   \item Calls \code{niarules::buildCoralPlots()} to compute the layout.
#' }
#'
#' This transformation is necessary to bridge the format expected by the layout algorithm
#' with the format used in rule mining packages like \pkg{arules}.
#'
#' @importFrom dplyr mutate select group_by arrange ungroup
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom niarules write_association_rules_to_csv buildCoralPlots
#' @importFrom utils read.csv
#' @export
build_coral_plots <- function(
    arules,
    edge_metric     = c("confidence","lift","support"),
    edge_gradient   = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"),
    node_color_by   = c("type","item","none"),
    node_colors     = NULL,
    palette_hcl_c   = 80,   # tweakable
    palette_hcl_l   = 50    # tweakable
) {
  edge_metric   <- match.arg(edge_metric)
  node_color_by <- match.arg(node_color_by)
  
  # ---- load rules (CSV in dev mode) ----
  if (is.null(arules)) {
    rules_df <- utils::read.csv("test.csv", stringsAsFactors = FALSE)
  } else {
    tmp_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_csv), add = TRUE)
    niarules::write_association_rules_to_csv(arules, file = tmp_csv, is_time_series = FALSE)
    rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  }
  
  # ---- one row per RULE (RHS kept intact) ----
  wdf <- .coral_build_wdf_prealloc(rules_df, .coral_split_outside_brackets)
  utils::write.csv(wdf, "wdf.csv")
  
  # sanity: LHS columns only
  lhs_cols <- grep("^lhs_", names(wdf), value = TRUE)
  if (length(lhs_cols)) {
    lhs_counts <- rowSums(!is.na(wdf[lhs_cols]))
    stopifnot(all(wdf$antecedent_length <= lhs_counts))
    stopifnot(!any(grepl(",\\s*[A-Za-z_.]", unlist(wdf[lhs_cols]), perl = TRUE), na.rm = TRUE))
  }
  
  # ---- grid size: one plot per *combined* RHS ----
  rhs_vec <- wdf$rhs
  rhs_vec <- rhs_vec[!is.na(rhs_vec) & nzchar(rhs_vec)]
  n_plots   <- max(1L, length(unique(rhs_vec)))
  grid_size <- ceiling(sqrt(n_plots))
  
  # ---- ask C++ for layout (let R handle colors; pass NULL for item/type colors) ----
  layout <- niarules::buildCoralPlots(
    wdf,
    grid_size     = grid_size,
    edge_gradient = edge_gradient,
    edge_metric   = edge_metric,
    item_types    = NULL,
    type_colors   = NULL
  )
  
  nodes <- layout$nodes
  edges <- layout$edges
  
  # C++ already provided: feature, kind, interval_low/high, incl_low/high,
  # category_val, interval_label, interval_label_short, plus color (currently black).
  # We recolor here per node_color_by.
  
  # ---- recolor nodes in R (optional) ----
  if (!is.null(nodes) && nrow(nodes)) {
    if (node_color_by == "none") {
      # leave whatever C++ gave (likely black)
    } else if (node_color_by == "type") {
      # color by parsed feature name (what used to be "type" conceptually)
      labs <- sort(unique(nodes$feature))
      pal  <- .coral_auto_fill_named_colors(labs, node_colors, palette_hcl_c, palette_hcl_l)
      col_map <- setNames(pal, labs)
      nodes$color <- unname(col_map[ nodes$feature ])
    } else { # "item"
      labs <- sort(unique(nodes$item))
      pal  <- .coral_auto_fill_named_colors(labs, node_colors, palette_hcl_c, palette_hcl_l)
      col_map <- setNames(pal, labs)
      nodes$color <- unname(col_map[ nodes$item ])
    }
  }
  
  # extra sanity for numeric intervals and coordinates (should already be OK from C++)
  if (!is.null(nodes) && nrow(nodes)) {
    nodes$interval_low  <- suppressWarnings(as.numeric(nodes$interval_low))
    nodes$interval_high <- suppressWarnings(as.numeric(nodes$interval_high))
    nodes$incl_low  <- as.logical(nodes$incl_low)
    nodes$incl_high <- as.logical(nodes$incl_high)
    
    bad_intervals <- with(nodes, kind == "numeric" & interval_low > interval_high)
    stopifnot(!any(bad_intervals, na.rm = TRUE))
    stopifnot(all(nodes$x >= 0 & nodes$x <= grid_size),
              all(nodes$z >= 0 & nodes$z <= grid_size))
  }
  
  list(
    nodes       = nodes,
    edges       = edges,
    edge_metric = layout$edge_metric,
    edge_range  = layout$edge_range,
    grid_size   = grid_size
  )
}

# ---- HELPERS (unchanged) ----

# short numeric formatter (still used in renderer)
.coral_fmt_num <- function(x, digits = 3) {
  ifelse(is.infinite(x),
         ifelse(x > 0, "\u221E", "-\u221E"),
         format(signif(x, digits), trim = TRUE, scientific = FALSE))
}

# split by commas that are OUTSIDE (), [], {}
.coral_split_outside_brackets <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  x <- gsub("^\\{|\\}$", "", x)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  
  d_par <- 0L; d_brk <- 0L; d_brc <- 0L
  parts <- character(0); buf <- character(0)
  
  flush_buf <- function() {
    s <- trimws(paste0(buf, collapse = ""))
    if (nzchar(s)) parts <<- c(parts, s)
    buf <<- character(0)
  }
  for (ch in chars) {
    if (ch == "(") d_par <- d_par + 1L else if (ch == ")") d_par <- max(0L, d_par - 1L)
    else if (ch == "[") d_brk <- d_brk + 1L else if (ch == "]") d_brk <- max(0L, d_brk - 1L)
    else if (ch == "{") d_brc <- d_brc + 1L else if (ch == "}") d_brc <- max(0L, d_brc - 1L)
    
    if (ch == "," && d_par == 0L && d_brk == 0L && d_brc == 0L) flush_buf() else buf <- c(buf, ch)
  }
  flush_buf()
  parts
}

# build the wide DF expected by C++ (one row per rule; RHS kept combined)
.coral_build_wdf_prealloc <- function(rules_df, split_fun = .coral_split_outside_brackets) {
  n <- nrow(rules_df)
  
  # first pass: split just to count LHS length (RHS kept combined)
  lhs_list <- vector("list", n)
  lhs_len  <- integer(n)
  for (i in seq_len(n)) {
    lhs <- split_fun(rules_df$Antecedent[i]); if (!length(lhs)) lhs <- character(0)
    lhs_list[[i]] <- lhs
    lhs_len[i]    <- length(lhs)
  }
  
  total_rows <- n
  max_lhs    <- max(0L, max(lhs_len, 0L))
  
  rule_id           <- integer(total_rows)
  support           <- numeric(total_rows)
  confidence        <- numeric(total_rows)
  lift              <- numeric(total_rows)
  rhs               <- character(total_rows)
  antecedent_length <- integer(total_rows)
  lhs_mat <- if (max_lhs > 0L) matrix(NA_character_, nrow = total_rows, ncol = max_lhs) else NULL
  if (max_lhs > 0L) colnames(lhs_mat) <- paste0("lhs_", seq_len(max_lhs))
  
  for (i in seq_len(n)) {
    rule_id[i]           <- i
    support[i]           <- rules_df$Support[i]
    confidence[i]        <- rules_df$Confidence[i]
    lift[i]              <- rules_df$Fitness[i]
    # keep combined RHS (trim outer braces but do NOT split)
    rhs_i <- trimws(gsub("^\\{|\\}$", "", rules_df$Consequence[i]))
    rhs[i] <- rhs_i
    antecedent_length[i] <- length(lhs_list[[i]])
    if (max_lhs > 0L && length(lhs_list[[i]])) {
      lhs_mat[i, seq_along(lhs_list[[i]])] <- lhs_list[[i]]
    }
  }
  
  df <- data.frame(rule_id, support, confidence, lift, rhs, antecedent_length,
                   stringsAsFactors = FALSE)
  if (max_lhs > 0L) df <- cbind(df, as.data.frame(lhs_mat, stringsAsFactors = FALSE))
  
  # basic sanity
  stopifnot(is.integer(df$rule_id), !anyDuplicated(df$rule_id))
  df
}
# color helper: honor user map first, auto-fill the rest with HCL
.coral_auto_fill_named_colors <- function(labels, user_map = NULL, hcl_c = 80, hcl_l = 50) {
  out <- rep(NA_character_, length(labels)); names(out) <- labels
  if (!is.null(user_map)) {
    m <- intersect(names(user_map), labels)
    out[m] <- as.character(user_map[m])
  }
  miss <- which(is.na(out) | !nzchar(out))
  if (length(miss)) {
    k <- length(miss)
    out[miss] <- grDevices::hcl(h = seq(15, 375, length.out = k + 1)[1:k], c = hcl_c, l = hcl_l)
  }
  unname(out)
}