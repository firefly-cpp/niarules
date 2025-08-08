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
    node_colors     = NULL  # optional named vector; names depend on node_color_by
) {
  edge_metric   <- match.arg(edge_metric)
  node_color_by <- match.arg(node_color_by)
  
  # helper: split on commas that are OUTSIDE parenthese
  fmt_num <- function(x, digits = 3) {
    ifelse(is.infinite(x),
           ifelse(x > 0, "\u221E", "-\u221E"),
           format(signif(x, digits), trim = TRUE, scientific = FALSE))
  }
  
  make_short_label <- function(row) {
    if (row[["kind"]] == "numeric") {
      paste0(
        row[["type"]], " ",
        if (isTRUE(row[["incl_low"]])) "[" else "(",
        fmt_num(row[["interval_low"]]), ", ",
        fmt_num(row[["interval_high"]]),
        if (isTRUE(row[["incl_high"]])) "]" else ")"
      )
    } else if (row[["kind"]] == "categorical") {
      paste0(row[["type"]], " = ", row[["category_val"]])
    } else {
      row[["item"]]
    }
  }
  
  
  parse_interval_info <- function(item_label) {
    x <- trimws(item_label)
    
    # If this string actually contains multiple top-level items, bail out early
    if (length(split_outside_brackets(x)) > 1L)
      return(list(kind="unknown", type=sub("\\s*\\(.*$","", x),
                  low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
                  value=NA_character_, interval_label=x))
    
    # Feature name: everything up to first bracket or operator
    feat <- trimws(sub("\\s*([\\(\\[]|<=|>=|=|<|>|\\s+in\\s+|%in%).*$", "", x, perl=TRUE))
    
    # ---------- (A) tolerant bracket interval: (..., ...) or [..., ...] ----------
    # Find first opening bracket and its matching closer
    open_pos <- regexpr("[\\(\\[]", x, perl=TRUE)
    if (open_pos > 0) {
      open_chr <- substr(x, open_pos, open_pos)
      close_chr <- if (open_chr == "(") ")" else "]"
      # take everything from the opening bracket to the last matching closer
      close_pos <- regexpr(paste0("\\", close_chr, "(?!.*\\", close_chr, ")"), x, perl=TRUE)
      if (close_pos > 0 && close_pos > open_pos) {
        inside <- substr(x, open_pos + 1L, close_pos - 1L)
        # split on the FIRST comma only
        comma_pos <- regexpr(",", inside, fixed=TRUE)
        if (comma_pos > 0) {
          lo <- trimws(substr(inside, 1L, comma_pos - 1L))
          hi <- trimws(substr(inside, comma_pos + 1L, nchar(inside)))
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
    
    # ---------- (B) relational: Feature <= v, >=, <, >, = v ----------
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
    
    # ---------- (C) set: Feature in {A,B} ----------
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
    
    # Fallback
    list(kind="unknown", type=feat,
         low=NA_real_, high=NA_real_, incl_low=NA, incl_high=NA,
         value=NA_character_, interval_label=x)
  }
  
  split_outside_brackets <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    x <- gsub("^\\{|\\}$", "", x)  # strip outer braces around the whole antecedent if present
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    
    depth_paren <- 0L  # ()
    depth_brack <- 0L  # []
    depth_brace <- 0L  # {}
    
    parts <- character(0)
    buf <- character(0)
    
    flush_buf <- function() {
      s <- trimws(paste0(buf, collapse = ""))
      if (nzchar(s)) parts <<- c(parts, s)
      buf <<- character(0)
    }
    
    for (ch in chars) {
      if (ch == "(") depth_paren <- depth_paren + 1L
      else if (ch == ")") depth_paren <- max(0L, depth_paren - 1L)
      else if (ch == "[") depth_brack <- depth_brack + 1L
      else if (ch == "]") depth_brack <- max(0L, depth_brack - 1L)
      else if (ch == "{") depth_brace <- depth_brace + 1L
      else if (ch == "}") depth_brace <- max(0L, depth_brace - 1L)
      
      if (ch == "," && depth_paren == 0L && depth_brack == 0L && depth_brace == 0L) {
        flush_buf()
      } else {
        buf <- c(buf, ch)
      }
    }
    flush_buf()
    parts
  }
  
  split_outside_parens <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    x <- gsub("^\\{|\\}$", "", x)  # strip outer braces from "{...}"
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    depth <- 0L
    parts <- character(0)
    buf <- character(0)
    for (ch in chars) {
      if (ch == "(") depth <- depth + 1L
      if (ch == ")" && depth > 0L) depth <- depth - 1L
      if (ch == "," && depth == 0L) {
        parts <- c(parts, trimws(paste0(buf, collapse = "")))
        buf <- character(0)
      } else {
        buf <- c(buf, ch)
      }
    }
    parts <- c(parts, trimws(paste0(buf, collapse = "")))
    parts[nzchar(parts)]
  }
  
  # clean feature name from an item label like:
  # "Length (0.63, 0.76)", "Sex = M", "Diameter>=0.4", "Class in {A,B}"
  parse_feature <- function(x) {
    x <- trimws(x)
    # drop everything starting at a parenthesis if present (interval form)
    x <- sub("\\s*\\(.*$", "", x)
    # then drop operator and RHS if present
    sub("\\s*(<=|>=|=|<|>|\\s+in\\s+|%in%).*$", "", x, perl = TRUE)
  }
  
  #tmp_csv <- tempfile(fileext = ".csv")
  #on.exit(unlink(tmp_csv), add = TRUE)
  
  #niarules::write_association_rules_to_csv(
  #  arules,
  #  file           = tmp_csv,
  #  is_time_series = FALSE
  #)
  
  #rules_df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
  
  #alt (since i dont want to mine all the time)
  rules_df <- utils::read.csv("test.csv", stringsAsFactors = FALSE)
  
  preflight_rules <- function(rules_df, split_fun) {
    n <- nrow(rules_df)
    lhs_len <- integer(n); rhs_len <- integer(n)
    for (i in seq_len(n)) {
      lhs <- split_fun(rules_df$Antecedent[i]); if (!length(lhs)) lhs <- NA_character_
      rhs <- split_fun(rules_df$Consequence[i]); if (!length(rhs)) rhs <- NA_character_
      lhs_len[i] <- length(lhs); rhs_len[i] <- length(rhs)
    }
    
    total_rows <- sum(pmax(rhs_len, 1L))
    max_lhs    <- max(pmax(lhs_len, 1L))
    cat(
      "#rules:", n, "\n",
      "max_lhs:", max_lhs, "  (95% quantile:", quantile(lhs_len, 0.95), ")\n",
      "max_rhs:", max(rhs_len), "  (95% quantile:", quantile(rhs_len, 0.95), ")\n",
      "total_rows after RHS expansion:", total_rows, "\n",
      "estimated cells in wide matrix:", format(total_rows * max_lhs, big.mark=","), "\n",
      sep=""
    )
    
    list(lhs_len=lhs_len, rhs_len=rhs_len, total_rows=total_rows, max_lhs=max_lhs)
  }
  
  #pf <- preflight_rules(rules_df, split_outside_brackets)
  wdf <- build_wdf_prealloc(rules_df, split_outside_brackets)
  
  # Minimal sanity checks (cheap)
  stopifnot(is.integer(wdf$rule_id))                       # must be integer
  stopifnot(!anyDuplicated(wdf$rule_id))
  stopifnot(!any(grepl(",\\s*[A-Za-z_.]", wdf$rhs, perl=TRUE), na.rm=TRUE))
  
  lhs_cols <- grep("^lhs_", names(wdf), value = TRUE)
  if (length(lhs_cols)) {
    # Ensure each row's antecedent_length <= number of non-NA lhs_j in that row
    lhs_counts <- rowSums(!is.na(wdf[lhs_cols]))
    stopifnot(all(wdf$antecedent_length <= lhs_counts))
  }
  
  # 2) No composite tokens left in LHS or RHS
  lhs_mat <- wdf[, grepl("^lhs_", names(wdf)), drop = FALSE]
  stopifnot(!any(grepl(",\\s*[A-Za-z_.]", unlist(lhs_mat), perl = TRUE), na.rm = TRUE))
  stopifnot(!any(grepl(",\\s*[A-Za-z_.]", wdf$rhs,            perl = TRUE), na.rm = TRUE))
  
  # ---- build item_types from labels without breaking on bound-commas ----
  # ---- collect items + derive "type" (feature) ----
  lhs_items <- wdf %>%
    dplyr::select(dplyr::starts_with("lhs_")) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "k", values_to = "item") %>%
    dplyr::filter(!is.na(item)) %>%
    dplyr::pull(item)
  all_items <- unique(c(lhs_items, wdf$rhs))
  
  # Build type table (same as before)
  item_types <- data.frame(
    item = all_items,
    type = vapply(all_items, \(s) sub("\\s*(<=|>=|=|<|>|\\s+in\\s+|%in%).*$","",
                                      sub("\\s*\\(.*$","", trimws(s)), perl=TRUE),
                  character(1)),
    stringsAsFactors = FALSE
  )
  
  # Parse interval/category info for each item
  parsed <- lapply(all_items, parse_interval_info)
  interval_df <- do.call(rbind, lapply(seq_along(all_items), function(i) {
    p <- parsed[[i]]
    data.frame(
      item          = all_items[i],
      type          = p$type,
      kind          = p$kind,
      interval_low  = p$low,
      interval_high = p$high,
      incl_low      = p$incl_low,
      incl_high     = p$incl_high,
      category_val  = p$value,
      interval_label= p$interval_label,
      stringsAsFactors = FALSE
    )
  }))
  
  # ---- Decide the label domain we’re coloring over ----
  if (node_color_by == "none") {
    # don’t send any type info; nodes will be black
    item_types_df <- NULL
    type_colors_df <- NULL
  } else if (node_color_by == "type") {
    labels <- sort(unique(item_types$type))
    # If user provided named colors for some/all types, honor them; fill the rest automatically
    pal <- auto_fill_named_colors(labels, node_colors)
    item_types_df <- item_types
    type_colors_df <- data.frame(type = labels, color = pal, stringsAsFactors = FALSE)
  } else { # node_color_by == "item"
    labels <- sort(unique(item_types$item))
    pal <- auto_fill_named_colors(labels, node_colors)
    # Trick: tell the C++ wrapper that "type" == "item" by setting type = item
    item_types_df <- transform(item_types, type = item)
    type_colors_df <- data.frame(type = labels, color = pal, stringsAsFactors = FALSE)
  }
  
  # ---- Grid size (robust) ----
  # Compute grid_size robustly
  rhs_vec <- as.character(wdf$rhs)
  rhs_vec <- rhs_vec[!is.na(rhs_vec) & nzchar(rhs_vec)]
  n_plots   <- max(1L, length(unique(rhs_vec)))
  grid_size <- ceiling(sqrt(n_plots))
  
  layout <- niarules::buildCoralPlots(
    wdf,
    grid_size     = grid_size,
    edge_gradient = edge_gradient,
    edge_metric   = edge_metric,
    item_types    = item_types_df,   # possibly NULL if node_color_by="none"
    type_colors   = type_colors_df   # possibly NULL
  )
  
  # <-- NEW: attach interval info to nodes by item label
  nodes_aug <- dplyr::left_join(
    layout$nodes,
    dplyr::select(
      interval_df,
      item,            # join key
      type, kind,      # keep one 'type' (the parsed feature)
      interval_low, interval_high, incl_low, incl_high,
      category_val, interval_label
    ),
    by = "item"
  )
  
  # ensure proper types
  nodes_aug$interval_low  <- suppressWarnings(as.numeric(nodes_aug$interval_low))
  nodes_aug$interval_high <- suppressWarnings(as.numeric(nodes_aug$interval_high))
  nodes_aug$incl_low      <- as.logical(nodes_aug$incl_low)
  nodes_aug$incl_high     <- as.logical(nodes_aug$incl_high)
  
  # short number formatter (3 sig figs; no trailing zeros)
  fmt_num <- function(x, digits = 3) {
    ifelse(is.infinite(x),
           ifelse(x > 0, "\u221E", "-\u221E"),
           format(signif(x, digits), trim = TRUE, scientific = FALSE))
  }
  
  # build short labels WITH feature name and correct brackets
  nodes_aug$interval_label_short <- ifelse(
    nodes_aug$kind == "numeric",
    paste0(
      nodes_aug$type, " ",
      ifelse(isTRUE(nodes_aug$incl_low), "[", "("),
      fmt_num(nodes_aug$interval_low, 3), ", ",
      fmt_num(nodes_aug$interval_high, 3),
      ifelse(isTRUE(nodes_aug$incl_high), "]", ")")
    ),
    ifelse(
      nodes_aug$kind == "categorical",
      paste0(nodes_aug$type, " = ", nodes_aug$category_val),
      nodes_aug$item
    )
  )
  
  list(
    nodes       = nodes_aug,          # includes: color (from C++), type, interval_low/high, interval_label, kind
    edges       = layout$edges,
    edge_metric = layout$edge_metric,
    edge_range  = layout$edge_range,
    grid_size   = grid_size
  )
}

build_wdf_prealloc <- function(rules_df, split_fun) {
  n <- nrow(rules_df)
  
  # First pass: split and measure
  lhs_list <- vector("list", n)
  rhs_list <- vector("list", n)
  lhs_len  <- integer(n)
  rhs_len  <- integer(n)
  for (i in seq_len(n)) {
    lhs <- split_fun(rules_df$Antecedent[i]); if (!length(lhs)) lhs <- character(0)
    rhs <- split_fun(rules_df$Consequence[i]); if (!length(rhs)) rhs <- character(0)
    lhs_list[[i]] <- lhs; rhs_list[[i]] <- rhs
    lhs_len[i] <- length(lhs); rhs_len[i] <- length(rhs)
  }
  
  total_rows <- sum(pmax(rhs_len, 1L))
  max_lhs    <- max(0L, max(lhs_len, 0L))
  
  # Preallocate
  rule_id           <- integer(total_rows)            # <-- NEW: integer
  support           <- numeric(total_rows)
  confidence        <- numeric(total_rows)
  lift              <- numeric(total_rows)
  rhs               <- character(total_rows)
  antecedent_length <- integer(total_rows)
  lhs_mat           <- if (max_lhs > 0L) matrix(NA_character_, nrow = total_rows, ncol = max_lhs) else NULL
  if (max_lhs > 0L) colnames(lhs_mat) <- paste0("lhs_", seq_len(max_lhs))
  
  row <- 0L
  next_id <- 0L                                     # <-- NEW: sequential integer ids
  for (i in seq_len(n)) {
    supp <- rules_df$Support[i]
    conf <- rules_df$Confidence[i]
    lft  <- rules_df$Fitness[i]
    lhs  <- lhs_list[[i]]
    rhs_items <- rhs_list[[i]]
    
    if (!length(rhs_items)) rhs_items <- NA_character_
    
    for (r in seq_along(rhs_items)) {
      row     <- row + 1L
      next_id <- next_id + 1L
      rule_id[row]           <- next_id              # <-- NEW: integer id
      support[row]           <- supp
      confidence[row]        <- conf
      lift[row]              <- lft
      rhs[row]               <- trimws(gsub("^\\{|\\}$", "", rhs_items[[r]]))
      
      if (max_lhs > 0L) {
        if (length(lhs)) lhs_mat[row, seq_along(lhs)] <- lhs
        # set the actual, per-row count of LHS items that we populated
        antecedent_length[row] <- length(lhs)        # <-- number of non-NA lhs_j we just filled
      } else {
        antecedent_length[row] <- 0L
      }
    }
  }
  
  df <- data.frame(
    rule_id, support, confidence, lift, rhs, antecedent_length,
    stringsAsFactors = FALSE
  )
  if (max_lhs > 0L) df <- cbind(df, as.data.frame(lhs_mat, stringsAsFactors = FALSE))
  df
}

auto_fill_named_colors <- function(labels, user_map = NULL) {
  # user_map can be named vector c("LabelA"="#ff...", "LabelB"="#00...")
  out <- rep(NA_character_, length(labels)); names(out) <- labels
  if (!is.null(user_map)) {
    # only use entries where the name matches a label
    m <- intersect(names(user_map), labels)
    out[m] <- as.character(user_map[m])
  }
  # fill remaining with a distinct HCL palette
  missing_idx <- which(is.na(out) | !nzchar(out))
  if (length(missing_idx)) {
    k <- length(missing_idx)
    out[missing_idx] <- grDevices::hcl(
      h = seq(15, 375, length.out = k + 1)[1:k],
      c = 80, l = 50
    )
  }
  unname(out)
}