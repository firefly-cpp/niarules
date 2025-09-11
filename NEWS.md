# niarules 0.3.1

## New (experimental)
- **`render_coral_rgl_experimental()`**: a feature-rich 3D renderer for coral plots.  
  Theming, legends, and flexible mappings for edge width/color/alpha.  
  _Status: experimental — arguments and defaults may change before 0.4.0._
- **`metric_domains()`**: compute [min, max] ranges for rule metrics (`support`, `confidence`, `lift`)
  from a `parsed` object or a data frame. _Status: experimental._
- **`coral_list_themes()`** and **`coral_get_theme()`**: list and retrieve theme objects
  for the experimental renderer. _Status: experimental._

## Enhancements
- **`build_coral_plots()`**
  - New optional arguments: `bin_breaks`, `bin_digits` (defaults preserve prior behavior).
  - Returns additional node fields: `node_id`, `is_root`, `coral_id`, `interval_brackets`, `bin_index`.
  - Adds a new return component **`bin_legend`** (or `NULL`) to describe bins.
  - The original `nodes`, `edges`, and `grid_size` components are unchanged.
- **`parse_rules()`**
  - More permissive input: accepts common column aliases (`lift`/`Fitness`, `lhs`/`rhs`, case-insensitive).
  - Output now carries class **`"parsed"`** to enable S3 methods like `metric_domains(parsed)`.

## Bug fixes
- **Interval parsing (C++)**: mixed bracket intervals like `[lo, hi)` and `(lo, hi]` are parsed correctly;
  bracketed intervals now consistently use `rel_op = "in"`.
- **LHS parsing (C++)**: single outer braces are stripped before splitting, so `{A,B}` is treated as `A, B`.

## Internal
- Rcpp entry points (`build_layout_cpp()`, `parse_rules_cpp()`) unchanged.
