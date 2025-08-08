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
    palette_hcl_c   = 80,
    palette_hcl_l   = 50
) {
  edge_metric   <- match.arg(edge_metric)
  node_color_by <- match.arg(node_color_by)
  
  if (is.null(arules)) {
    rules_df <- utils::read.csv("test.csv", stringsAsFactors = FALSE)
  } else {
    tmp_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_csv), add = TRUE)
    niarules::write_association_rules_to_csv(arules, file = tmp_csv, is_time_series = FALSE)
    rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  }
  
  # one row per RULE (combined RHS kept intact)
  wdf <- .coral_build_wdf_prealloc(rules_df, .coral_split_outside_brackets)
  utils::write.csv(wdf, "wdf.csv")
  
  # --- sanity checks (LHS only; RHS can be composite now) ---
  lhs_cols <- grep("^lhs_", names(wdf), value = TRUE)
  if (length(lhs_cols)) {
    lhs_counts <- rowSums(!is.na(wdf[lhs_cols]))
    stopifnot(all(wdf$antecedent_length <= lhs_counts))
    stopifnot(!any(grepl(",\\s*[A-Za-z_.]", unlist(wdf[lhs_cols]), perl = TRUE), na.rm = TRUE))
  }
  
  # --- build item_types domain (split combined RHS only for metadata) ---
  lhs_items <- if (length(lhs_cols)) {
    tidyr::pivot_longer(wdf[lhs_cols], dplyr::everything(),
                        names_to = "k", values_to = "item") |>
      dplyr::filter(!is.na(.data$item)) |>
      dplyr::pull(.data$item)
  } else character(0)
  
  rhs_items <- unlist(lapply(wdf$rhs, function(s) .coral_split_outside_brackets(s)))
  rhs_items <- trimws(rhs_items[nzchar(rhs_items)])
  
  all_items <- unique(c(lhs_items, rhs_items))
  
  item_types <- data.frame(
    item = all_items,
    type = vapply(all_items, \(s)
                  sub("\\s*(<=|>=|=|<|>|\\s+in\\s+|%in%).*$", "",
                      sub("\\s*\\(.*$","", trimws(s)), perl = TRUE),
                  character(1L)),
    stringsAsFactors = FALSE
  )
  
  parsed <- lapply(all_items, .coral_parse_interval_info)
  interval_df <- do.call(rbind, lapply(seq_along(all_items), function(i) {
    p <- parsed[[i]]
    data.frame(
      item           = all_items[i],
      type           = p$type,
      kind           = p$kind,
      interval_low   = p$low,
      interval_high  = p$high,
      incl_low       = p$incl_low,
      incl_high      = p$incl_high,
      category_val   = p$value,
      interval_label = p$interval_label,
      stringsAsFactors = FALSE
    )
  }))
  
  # --- node-coloring domain ---
  if (node_color_by == "none") {
    item_types_df <- NULL
    type_colors_df <- NULL
  } else if (node_color_by == "type") {
    labels <- sort(unique(item_types$type))
    pal <- .coral_auto_fill_named_colors(labels, node_colors, palette_hcl_c, palette_hcl_l)
    item_types_df <- item_types
    type_colors_df <- data.frame(type = labels, color = pal, stringsAsFactors = FALSE)
  } else { # "item"
    labels <- sort(unique(item_types$item))
    pal <- .coral_auto_fill_named_colors(labels, node_colors, palette_hcl_c, palette_hcl_l)
    item_types_df <- transform(item_types, type = item)
    type_colors_df <- data.frame(type = labels, color = pal, stringsAsFactors = FALSE)
  }
  
  # --- robust grid size: one plot per *combined* RHS ---
  rhs_vec <- wdf$rhs
  rhs_vec <- rhs_vec[!is.na(rhs_vec) & nzchar(rhs_vec)]
  n_plots   <- max(1L, length(unique(rhs_vec)))
  grid_size <- ceiling(sqrt(n_plots))
  
  # --- layout in C++ (it can split rhs internally for roots; colors by type there) ---
  layout <- niarules::buildCoralPlots(
    wdf,
    grid_size     = grid_size,
    edge_gradient = edge_gradient,
    edge_metric   = edge_metric,
    item_types    = item_types_df,
    type_colors   = type_colors_df
  )
  
  # --- enrich nodes with parsed interval info ---
  nodes_aug <- dplyr::left_join(
    layout$nodes,
    dplyr::select(
      interval_df,
      item, type, kind,
      interval_low, interval_high, incl_low, incl_high,
      category_val, interval_label
    ),
    by = "item"
  )
  
  nodes_aug$interval_low  <- suppressWarnings(as.numeric(nodes_aug$interval_low))
  nodes_aug$interval_high <- suppressWarnings(as.numeric(nodes_aug$interval_high))
  nodes_aug$incl_low      <- as.logical(nodes_aug$incl_low)
  nodes_aug$incl_high     <- as.logical(nodes_aug$incl_high)
  
  nodes_aug$interval_label_short <- ifelse(
    nodes_aug$kind == "numeric",
    paste0(
      nodes_aug$type, " ",
      ifelse(isTRUE(nodes_aug$incl_low), "[", "("),
      .coral_fmt_num(nodes_aug$interval_low,  3), ", ",
      .coral_fmt_num(nodes_aug$interval_high, 3),
      ifelse(isTRUE(nodes_aug$incl_high), "]", ")")
    ),
    ifelse(
      nodes_aug$kind == "categorical",
      paste0(nodes_aug$type, " = ", nodes_aug$category_val),
      nodes_aug$item
    )
  )
  
  bad_intervals <- with(nodes_aug, kind == "numeric" & interval_low > interval_high)
  stopifnot(!any(bad_intervals, na.rm = TRUE))
  stopifnot(all(nodes_aug$x >= 0 & nodes_aug$x <= grid_size),
            all(nodes_aug$z >= 0 & nodes_aug$z <= grid_size))
  
  list(
    nodes       = nodes_aug,
    edges       = layout$edges,
    edge_metric = layout$edge_metric,
    edge_range  = layout$edge_range,
    grid_size   = grid_size
  )
}

# HELPERS

# short numeric formatter (used for labels)
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

# parse a single item label into kind/type/interval/category info
.coral_parse_interval_info <- function(item_label) {
  x <- trimws(item_label)
  
  # early out if this actually contains multiple top-level items
  if (length(.coral_split_outside_brackets(x)) > 1L) {
    feat <- trimws(sub("\\s*\\(.*$", "", x))
    return(list(kind="unknown", type=feat, low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
                value=NA_character_, interval_label=x))
  }
  
  # feature name up to first bracket or operator
  feat <- trimws(sub("\\s*([\\(\\[]|<=|>=|=|<|>|\\s+in\\s+|%in%).*$", "", x, perl=TRUE))
  
  # tolerant brackets:  (..., ...)  or  [..., ...]
  open_pos <- regexpr("[\\(\\[]", x, perl=TRUE)
  if (open_pos > 0) {
    open_chr  <- substr(x, open_pos, open_pos)
    close_chr <- if (open_chr == "(") ")" else "]"
    close_pos <- regexpr(paste0("\\", close_chr, "(?!.*\\", close_chr, ")"), x, perl=TRUE)
    if (close_pos > 0 && close_pos > open_pos) {
      inside <- substr(x, open_pos + 1L, close_pos - 1L)
      cp <- regexpr(",", inside, fixed=TRUE)
      if (cp > 0) {
        lo <- trimws(substr(inside, 1L, cp - 1L))
        hi <- trimws(substr(inside, cp + 1L, nchar(inside)))
        low  <- suppressWarnings(as.numeric(lo))
        high <- suppressWarnings(as.numeric(hi))
        if (!is.na(low) && !is.na(high)) {
          incl_low  <- (open_chr == "[")
          incl_high <- (close_chr == "]")
          return(list(
            kind="numeric", type=feat,
            low=low, high=high, incl_low=incl_low, incl_high=incl_high,
            value=NA_character_,
            interval_label = sprintf("%s %s%s, %s%s", feat, open_chr, lo, hi, close_chr)
          ))
        }
      }
    }
  }
  
  # relational:  Feature <= v, >= v, < v, > v, = v
  m <- regexec("^\\s*([^<>=%]+?)\\s*(<=|>=|=|<|>)\\s*([^,}]+)\\s*$", x)
  g <- regmatches(x, m)[[1]]
  if (length(g) == 4) {
    feat2 <- trimws(g[2]); op <- g[3]; val <- trimws(g[4])
    vnum <- suppressWarnings(as.numeric(val))
    if (!is.na(vnum) && op %in% c("<","<=",">",">=")) {
      if (op %in% c("<","<=")) {
        low <- -Inf; high <- vnum; incl_low <- FALSE; incl_high <- (op=="<=")
      } else {
        low <- vnum; high <-  Inf; incl_low <- (op==">="); incl_high <- FALSE
      }
      return(list(
        kind="numeric", type=feat2,
        low=low, high=high, incl_low=incl_low, incl_high=incl_high,
        value=NA_character_,
        interval_label = sprintf("%s %s %s", feat2, op, val)
      ))
    } else if (op == "=") {
      return(list(
        kind="categorical", type=feat2,
        low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
        value=val,
        interval_label = sprintf("%s = %s", feat2, val)
      ))
    }
  }
  
  # set:  Feature in {A,B}
  m <- regexec("^\\s*([^<>=%]+?)\\s*(?:in|%in%)\\s*\\{(.+)\\}\\s*$", x, perl=TRUE)
  g <- regmatches(x, m)[[1]]
  if (length(g) == 3) {
    feat2 <- trimws(g[2]); vals <- trimws(strsplit(g[3], ",")[[1]])
    return(list(
      kind="categorical", type=feat2,
      low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
      value=paste(vals, collapse=", "),
      interval_label=sprintf("%s in {%s}", feat2, paste(vals, collapse=", "))
    ))
  }
  
  # fallback
  list(kind="unknown", type=feat,
       low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
       value=NA_character_, interval_label=x)
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

# build the wide DF expected by C++ (one row per (rule, RHS item))
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
  
  stopifnot(is.integer(df$rule_id), !anyDuplicated(df$rule_id))
  df
}