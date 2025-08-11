#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <tuple>
#include <limits>
#include <cmath>

using namespace Rcpp;

#include "coral_plots.h"
#include "coral_layout_builder.h"
#include "string_splitter.h"

static int get_item_id(std::unordered_map<std::string,int>& m,
                       std::vector<std::string>& id2item,
                       const std::string& s) {
  auto it = m.find(s);
  if (it != m.end()) return it->second;
  int id = static_cast<int>(id2item.size());
  m.emplace(s, id);
  id2item.push_back(s);
  return id;
}

static std::vector<int> to_std_vec(const IntegerVector& v) {
  std::vector<int> out; out.reserve(v.size());
  for (int x : v) out.push_back(x);
  return out;
}

// small color helpers (same behavior as existing buildCoralPlots)
static std::tuple<int,int,int> hexToRGB(const std::string& hex) {
  int r=0,g=0,b=0;
  if (hex.size()==7 && hex[0]=='#') {
    r = std::stoi(hex.substr(1,2), nullptr, 16);
    g = std::stoi(hex.substr(3,2), nullptr, 16);
    b = std::stoi(hex.substr(5,2), nullptr, 16);
  } else if (hex.size()==9 && hex[0]=='#') {
    r = std::stoi(hex.substr(3,2), nullptr, 16);
    g = std::stoi(hex.substr(5,2), nullptr, 16);
    b = std::stoi(hex.substr(7,2), nullptr, 16);
  }
  return {r,g,b};
}

static std::string rgbToHex(int r,int g,int b) {
  char buf[8]; std::snprintf(buf, sizeof(buf), "#%02X%02X%02X", r,g,b);
  return std::string(buf);
}

static inline double clamp01(double t){ return t<0?0.0:(t>1?1.0:t); }
static inline double lerp(double a,double b,double t){ return a + (b-a)*t; }
static std::string samplePalette(double t, const std::vector<std::tuple<int,int,int>>& pal) {
  if (pal.empty()) return "#000000";
  if (pal.size()==1) {
    auto [r,g,b]=pal.front(); return rgbToHex(r,g,b);
  }
  t = clamp01(t);
  const double pos = t * (pal.size()-1);
  size_t i = static_cast<size_t>(std::floor(pos));
  size_t j = std::min(i+1, pal.size()-1);
  double f = pos - i;
  auto [r1,g1,b1] = pal[i];
  auto [r2,g2,b2] = pal[j];
  int r = static_cast<int>(std::round(lerp(r1,r2,f)));
  int g = static_cast<int>(std::round(lerp(g1,g2,f)));
  int b = static_cast<int>(std::round(lerp(b1,b2,f)));
  return rgbToHex(r,g,b);
}

static std::string feature_from_label_top_level_first(std::string s) {
    // take only the FIRST top-level token (outside (), [], {})
    auto parts = split_outside_brackets(s);
    std::string first = parts.empty() ? s : trim_copy(parts[0]);

    // strip operators/brackets after the feature name
    size_t n = first.size(), i = 0;
    for (; i < n; ++i) {
        char c = first[i];
        if (c == '(' || c == '[' || c == '<' || c == '>' || c == '=') break;
        // crude " in " / "%in%" handling
        if (c == ' ' && i + 1 < n && (first[i + 1] == 'i' || first[i + 1] == '%')) break;
    }
    std::string out = trim_copy(first.substr(0, i));
    if (!out.empty() && out.back() == ' ') out.pop_back();
    return out;
}

/// @brief Retrieves the integer ID for a given item string, adding it to the map and registry if not already present.
///
/// If the item exists in the lookup map, its ID is returned. Otherwise, a new ID is generated,
/// the item is added to both the lookup map and registry vector, and the new ID is returned.
///
/// @param lookup A map from item strings to integer IDs.
/// @param registry A vector maintaining the order and names of items (ID to string mapping).
/// @param item The item string to look up or insert.
/// @return The integer ID corresponding to the given item.
static int get_item_id_old(
    std::unordered_map<std::string, int> &lookup,
    std::vector<std::string> &registry,
    std::string const &item
) {
    if (const auto it = lookup.find(item); it != lookup.end())
        return it->second;
    const int new_id = static_cast<int>(registry.size());
    lookup[item] = new_id;
    registry.push_back(item);
    return new_id;
}

/// @brief Converts a wide-format R DataFrame of association rules into a vector of Rule objects.
///
/// This function expects the DataFrame to include hardcoded column names:
/// - "rule_id": Integer rule IDs
/// - "rhs": Consequent item (as string)
/// - "support", "confidence", "lift": Numeric measures
/// - "antecedent_length": Number of items in the antecedent
/// - "lhs_1", "lhs_2", ..., "lhs_k": Antecedent items (as strings)
///
/// Each unique item is assigned an integer ID using the provided lookup and registry.
///
/// @param df A Rcpp DataFrame containing the association rules in wide format.
/// @param item_to_id Map from item string to unique integer ID.
/// @param id_to_item Vector of item strings indexed by their assigned ID.
/// @return A vector of Rule objects representing the parsed rules.
static std::vector<coral_plots::Rule> df_to_rules(
    const DataFrame& df,
    std::unordered_map<std::string, int>& item_to_id,
    std::vector<std::string>& id_to_item
) {
    // required columns
    IntegerVector ids = df["rule_id"];
    CharacterVector rhs_chr = df["rhs"];
    NumericVector supports = df["support"];
    NumericVector confidences = df["confidence"];
    NumericVector lifts = df["lift"];
    IntegerVector lengths = df["antecedent_length"];

    const int n = ids.size();
    std::vector<coral_plots::Rule> out;
    out.reserve(n);

    // collect lhs_* columns once, sorted numerically
    std::vector<std::pair<int, std::string>> lhs_indexed;
    CharacterVector nms = df.names();
    for (int i = 0; i < nms.size(); ++i) {
        std::string s = Rcpp::as<std::string>(nms[i]);
        if (s.rfind("lhs_", 0) == 0) {
            // parse the number after "lhs_"
            int idx = std::atoi(s.c_str() + 4);
            lhs_indexed.emplace_back(idx, s);
        }
    }
    std::sort(lhs_indexed.begin(), lhs_indexed.end(),
        [](auto& a, auto& b) { return a.first < b.first; });

    // also materialize column vectors to avoid repeated df[...] lookups
    std::vector<CharacterVector> lhs_cols;
    lhs_cols.reserve(lhs_indexed.size());
    for (auto& kv : lhs_indexed) {
        lhs_cols.emplace_back(df[kv.second]);
    }

    for (int i = 0; i < n; ++i) {
        // RHS mapping (combined, normalized)
        std::string rhs_str = (rhs_chr[i] == NA_STRING) ? std::string()
            : Rcpp::as<std::string>(rhs_chr[i]);
        rhs_str = trim_copy(strip_outer_braces(rhs_str));
        const int rhs_id = get_item_id_old(item_to_id, id_to_item, rhs_str);

        // register individual RHS items (for stacked step-0 roots)
        for (const auto& sub : split_outside_brackets(rhs_str)) {
            (void)get_item_id_old(item_to_id, id_to_item, trim_copy(sub));
        }

        // LHS mapping: enforce atomic tokens per cell
        const int k = std::max(0, (int)lengths[i]);
        std::vector<int> ant;
        ant.reserve(k);

        const int avail = (int)lhs_cols.size();
        const int upto = std::min(k, avail);
        for (int j = 0; j < upto; ++j) {
            const CharacterVector& colV = lhs_cols[j];
            if (CharacterVector::is_na(colV[i]) || as<std::string>(colV[i]).empty()) continue;

            std::string cell = Rcpp::as<std::string>(colV[i]);
            auto parts = split_outside_brackets(cell);

            if (parts.empty()) continue;                  // blank/empty ? skip
            if (parts.size() > 1) {
                Rcpp::stop("Non-atomic LHS cell in %s (row %d): '%s'",
                    lhs_indexed[j].second.c_str(), i + 1, cell.c_str());
            }
            std::string token = trim_copy(parts[0]);
            /*int lhs_id = get_item_id_old(item_to_id, id_to_item, token);
            ant.push_back(lhs_id);*/

            int lhs_id = get_item_id_old(item_to_id, id_to_item, token);

            // sanity: the id maps back to the same token
            const std::string& roundtrip = id_to_item[lhs_id];
            if (split_outside_brackets(roundtrip).size() > 1) {
                Rcpp::stop("LHS token became non-atomic after ID mapping: '%s'", roundtrip.c_str());
            }
            ant.push_back(lhs_id);
        }

        coral_plots::Rule r;
        r.rule_id = ids[i];
        r.consequent = rhs_id;
        r.antecedent = std::move(ant);
        r.support = supports[i];
        r.confidence = confidences[i];
        r.lift = lifts[i];

        out.push_back(std::move(r));
    }
    return out;
}

//' @title Entry point for R to generate coral plot data from a set of association rules.
//'
//' @description This function takes a data frame of association rules and produces two data frames:
//' one for the nodes and one for the edges of a coral plot. It acts as a wrapper that:
//' \itemize{
//'    \item Converts the input R data frame to internal Rule objects.
//'    \item Constructs the coral layout by calling the lower-level `buildCoralPlots` function.
//'    \item Converts the resulting nodes and edges into R-compatible data frames.
//' }
//'
//' @param rulesDF A wide-format DataFrame representing association rules (as typically produced in R).
//' @param grid_size Number of grid cells per layout dimension to use during node positioning.
//'
//' @return A List containing two DataFrames:
//' \itemize{
//'    \item \code{edges}: DataFrame with start and end coordinates, line width, and color for each edge.
//'    \item \code{nodes}: DataFrame with position, radius, ID, and item label for each node.
//' }
//' @export
// [[Rcpp::export]]
List buildCoralPlots(const DataFrame& rulesDF, int grid_size,
    Rcpp::CharacterVector edge_gradient = Rcpp::CharacterVector::create("#2c7bb6", "#d7191c"),
    std::string edge_metric = "support",
    Rcpp::Nullable<Rcpp::DataFrame> item_types = R_NilValue,
    Rcpp::Nullable<Rcpp::DataFrame> type_colors = R_NilValue) {

    // --- helpers for color interpolation ---
    auto clamp01 = [](double t) { return t < 0 ? 0.0 : (t > 1 ? 1.0 : t); };

    auto hexToRGB = [](const std::string& hex) {
        // supports #RRGGBB or #AARRGGBB; ignores alpha
        int r = 0, g = 0, b = 0;
        if (hex.size() == 7 && hex[0] == '#') {
            r = std::stoi(hex.substr(1, 2), nullptr, 16);
            g = std::stoi(hex.substr(3, 2), nullptr, 16);
            b = std::stoi(hex.substr(5, 2), nullptr, 16);
        }
        else if (hex.size() == 9 && hex[0] == '#') {
            r = std::stoi(hex.substr(3, 2), nullptr, 16);
            g = std::stoi(hex.substr(5, 2), nullptr, 16);
            b = std::stoi(hex.substr(7, 2), nullptr, 16);
        }
        else {
            // default to black if weird input
            r = g = b = 0;
        }
        return std::tuple<int, int, int>(r, g, b);
        };

    auto rgbToHex = [](int r, int g, int b) {
        char buf[8];
        std::snprintf(buf, sizeof(buf), "#%02X%02X%02X", r, g, b);
        return std::string(buf);
        };

    auto lerp = [](double a, double b, double t) { return a + (b - a) * t; };

    auto samplePalette = [&](double t, const std::vector<std::tuple<int, int, int>>& pal) {
        if (pal.empty()) return std::string("#000000");
        if (pal.size() == 1) {
            auto [r, g, b] = pal.front();
            return rgbToHex(r, g, b);
        }
        t = clamp01(t);
        const double pos = t * (pal.size() - 1);
        const size_t i = static_cast<size_t>(std::floor(pos));
        const size_t j = std::min(i + 1, pal.size() - 1);
        const double f = pos - i;
        auto [r1, g1, b1] = pal[i];
        auto [r2, g2, b2] = pal[j];
        int r = static_cast<int>(std::round(lerp(r1, r2, f)));
        int g = static_cast<int>(std::round(lerp(g1, g2, f)));
        int b = static_cast<int>(std::round(lerp(b1, b2, f)));
        return rgbToHex(r, g, b);
        };
    // --- end helpers ---

    // ---- Build lookup: item -> type ----
    std::unordered_map<std::string, std::string> item2type;
    if (item_types.isNotNull()) {
        DataFrame it = item_types.get();
        CharacterVector items = it["item"];
        CharacterVector types = it["type"];
        for (int i = 0; i < items.size(); ++i) {
            item2type[as<std::string>(items[i])] = as<std::string>(types[i]);
        }
    }

    // ---- Build lookup: type -> color ----
    std::unordered_map<std::string, std::string> type2color;
    if (type_colors.isNotNull()) {
        DataFrame tc = type_colors.get();
        CharacterVector t = tc["type"];
        CharacterVector c = tc["color"];
        for (int i = 0; i < t.size(); ++i) {
            type2color[as<std::string>(t[i])] = as<std::string>(c[i]);
        }
    }

    std::unordered_map<std::string, int> item_to_id;
    std::vector<std::string> id_to_item;

    // convert wide data.frame into vector<Rule>
    std::vector<coral_plots::Rule> rules = df_to_rules(rulesDF, item_to_id, id_to_item);

    // call the layout algorithm
    std::vector<coral_plots::Edge> edges;
    std::vector<coral_plots::Node> nodes;
    coral_plots::buildCoralPlots(rules, edges, nodes, item_to_id, id_to_item, grid_size);

    // convert edges back to R
    // choose accessor for metric
    std::function<double(const coral_plots::Edge&)> getMetric;
    if (edge_metric == "confidence") {
        getMetric = [](const coral_plots::Edge& e) { return e.confidence; };
    }
    else if (edge_metric == "lift") {
        getMetric = [](const coral_plots::Edge& e) { return e.lift; };
    }
    else {
        edge_metric = "support";
        getMetric = [](const coral_plots::Edge& e) { return e.support; };
    }

    // build numeric ranges for normalization
    int E = static_cast<int>(edges.size());
    double minV = std::numeric_limits<double>::infinity();
    double maxV = -std::numeric_limits<double>::infinity();
    for (const auto& e : edges) {
        const double v = getMetric(e);
        if (v < minV) minV = v;
        if (v > maxV) maxV = v;
    }
    const bool constant = !(maxV > minV);

    // parse palette
    std::vector<std::tuple<int, int, int>> pal;
    pal.reserve(edge_gradient.size());
    for (auto& s : edge_gradient) pal.push_back(hexToRGB(as<std::string>(s)));

    // convert edges -> R
    NumericVector x_start(E), y_start(E), z_start(E), x_end(E), y_end(E), z_end(E), width(E);
    CharacterVector color(E);
    for (int i = 0; i < E; ++i) {
        x_start[i] = edges[i].x_start;
        y_start[i] = edges[i].y_start;
        z_start[i] = edges[i].z_start;
        x_end[i] = edges[i].x_end;
        y_end[i] = edges[i].y_end;
        z_end[i] = edges[i].z_end;
        width[i] = edges[i].line_width;

        double t = constant ? 0.5 : (getMetric(edges[i]) - minV) / (maxV - minV);
        color[i] = samplePalette(t, pal);
    }
    DataFrame edgesDF = DataFrame::create(
        _["x"] = x_start,
        _["y"] = y_start,
        _["z"] = z_start,
        _["x_end"] = x_end,
        _["y_end"] = y_end,
        _["z_end"] = z_end,
        _["width"] = width,
        _["color"] = color,
        _["stringsAsFactors"] = false
    );

    // convert nodes back to R
    // add type + color (by type)
    int N = static_cast<int>(nodes.size());
    NumericVector x(N), y(N), z(N), radius(N), x_offset(N), z_offset(N), interval_low(N), interval_high(N);
    IntegerVector id(N), step(N);
    LogicalVector incl_low(N), incl_high(N);
    CharacterVector item(N), color_type(N), node_color(N), feature(N), kind(N), category_val(N), interval_label(N), interval_label_short(N);

    for (int i = 0; i < N; ++i) {
        radius[i] = nodes[i].node_radius;
        x[i] = nodes[i].x; y[i] = nodes[i].y; z[i] = nodes[i].z;
        x_offset[i] = nodes[i].x_offset; z_offset[i] = nodes[i].z_offset;
        id[i] = nodes[i].item; step[i] = nodes[i].step;
        interval_low[i] = nodes[i].interval_low;
        interval_high[i] = nodes[i].interval_high;
        incl_low[i] = nodes[i].incl_low;
        incl_high[i] = nodes[i].incl_high;
        // harden against any composite label sneaking through
        feature[i] = feature_from_label_top_level_first(id_to_item[nodes[i].item]);
        kind[i] = nodes[i].kind;
        category_val[i] = nodes[i].category_val;
        interval_label[i] = nodes[i].interval_label;
        interval_label_short[i] = nodes[i].interval_label_short;

        const std::string item_s = id_to_item[nodes[i].item];
        item[i] = item_s;

        //Rcpp::Rcout << "item " << item_s << std::endl;

        // type lookup
        std::string t = "unknown";
        auto it = item2type.find(item_s);
        if (it != item2type.end()) t = it->second;
        color_type[i] = t;

        //Rcpp::Rcout << "node type " << t << std::endl;

        // color lookup by type (fallback to black)
        std::string col = "#000000";
        auto jt = type2color.find(t);
        if (jt != type2color.end()) col = jt->second;
        node_color[i] = col;

        //Rcpp::Rcout << "node color " << col << std::endl;
    }

    DataFrame nodesDF = DataFrame::create(
        _["x"] = x,
        _["y"] = y,
        _["z"] = z,
        _["x_offset"] = x_offset,
        _["z_offset"] = z_offset,
        _["radius"] = radius,
        _["id"] = id,
        _["item"] = item,
        _["type"] = color_type,
        _["color"] = node_color,
        _["step"] = step,
        _["feature"] = feature,
        _["kind"] = kind,
        _["interval_low"] = interval_low,
        _["interval_high"] = interval_high,
        _["incl_low"] = incl_low,
        _["incl_high"] = incl_high,
        _["category_val"] = category_val,
        _["interval_label"] = interval_label,
        _["interval_label_short"] = interval_label_short
    );

    // and done
    return List::create(
        _["edges"] = edgesDF,
        _["nodes"] = nodesDF,
        _["edge_metric"] = edge_metric,
        _["edge_range"] = NumericVector::create(minV, maxV)
    );
}

//' @export
// [[Rcpp::export]]
Rcpp::List build_layout_cpp(Rcpp::List parsed,
                            int grid_size,
                            std::string lhs_sort = "confidence",
                            std::string edge_metric = "support",
                            Rcpp::CharacterVector edge_gradient = Rcpp::CharacterVector::create("#2c7bb6","#d7191c")) {
  // ---- unpack parsed ----
  //DataFrame items = parsed["items"];
  //DataFrame rules = parsed["rules"];
  Rcpp::DataFrame items = Rcpp::as<Rcpp::DataFrame>(parsed["items"]);
  Rcpp::DataFrame rules = Rcpp::as<Rcpp::DataFrame>(parsed["rules"]);


  IntegerVector item_id = items["item_id"];
  CharacterVector item_label = items["label"];

  const int M = items.nrow();

  // id_to_item seeded with the existing atomic items (0-based!)
  std::vector<std::string> id_to_item(M);
  for (int i=0;i<M;++i) id_to_item[i] = as<std::string>(item_label[i]);
  std::unordered_map<std::string,int> item_to_id;
  for (int i=0;i<M;++i) item_to_id[id_to_item[i]] = i;

  // rules columns
  IntegerVector rule_id      = rules["rule_id"];
  NumericVector support_col  = rules["support"];
  NumericVector conf_col     = rules["confidence"];
  NumericVector lift_col     = rules["lift"];
  List         lhs_ids_list  = rules["lhs_item_ids"];
  List         rhs_ids_list  = rules["rhs_item_ids"];

  const int N = rules.nrow();
  std::vector<coral_plots::Rule> cpp_rules;
  cpp_rules.reserve(N);

  // build composite RHS labels so multiple RHS items share a single "root" id
  for (int i=0;i<N;++i) {
    // LHS
    std::vector<int> lhs_vec;
    if (lhs_ids_list[i] != R_NilValue) {
      lhs_vec = to_std_vec(IntegerVector(lhs_ids_list[i]));
    }

    // RHS composite label
    std::string rhs_label;
    if (rhs_ids_list[i] != R_NilValue) {
      auto rhs_vec = to_std_vec(IntegerVector(rhs_ids_list[i]));
      for (size_t k=0;k<rhs_vec.size();++k) {
        if (k) rhs_label += ", ";
        int id = rhs_vec[k];
        if (id < 0 || id >= M) stop("build_layout_cpp: rhs item_id out of range.");
        rhs_label += id_to_item[id];
      }
    }

    int rhs_id = get_item_id(item_to_id, id_to_item, rhs_label);
    coral_plots::Rule r;
    r.rule_id    = rule_id[i];
    r.consequent = rhs_id;
    r.antecedent = std::move(lhs_vec);
    r.support    = support_col[i];
    r.confidence = conf_col[i];
    r.lift       = lift_col[i];
    cpp_rules.push_back(std::move(r));
  }

  // ---- metric selector for LHS sorting ----
  int metric_to_use = 0; // 0=confidence, 1=support, 2=lift
  if (lhs_sort == "support") metric_to_use = 1;
  else if (lhs_sort == "lift") metric_to_use = 2;

  // ---- run layout ----
  std::vector<coral_plots::Edge> edges;
  std::vector<coral_plots::Node> nodes;

  // New overload (see header/cpp tweak below) that takes metric_to_use
  coral_plots::CoralLayoutBuilder::build(
    cpp_rules, grid_size, 0.5, id_to_item, nodes, edges, metric_to_use
  );

  // ---- edges: width + color via edge_metric + gradient ----
  std::function<double(const coral_plots::Edge&)> getMetric;
  if (edge_metric == "confidence")      getMetric = [](const coral_plots::Edge& e){return e.confidence;};
  else if (edge_metric == "lift")       getMetric = [](const coral_plots::Edge& e){return e.lift;};
  else { edge_metric = "support";       getMetric = [](const coral_plots::Edge& e){return e.support;}; }

  int E = static_cast<int>(edges.size());
  double minV = std::numeric_limits<double>::infinity();
  double maxV = -std::numeric_limits<double>::infinity();
  for (const auto& e : edges) { double v = getMetric(e); if (v < minV) minV = v; if (v > maxV) maxV = v; }
  bool constant = !(maxV > minV);

  std::vector<std::tuple<int,int,int>> pal; pal.reserve(edge_gradient.size());
  for (auto& s : edge_gradient) pal.push_back(hexToRGB(as<std::string>(s)));

  NumericVector x(E), y(E), z(E), x_end(E), y_end(E), z_end(E), width(E);
  CharacterVector color(E);
  for (int i=0;i<E;++i) {
    x[i]     = edges[i].x_start;
    y[i]     = edges[i].y_start;
    z[i]     = edges[i].z_start;
    x_end[i] = edges[i].x_end;
    y_end[i] = edges[i].y_end;
    z_end[i] = edges[i].z_end;
    width[i] = edges[i].line_width;

    double t = constant ? 0.5 : (getMetric(edges[i]) - minV) / (maxV - minV);
    color[i] = samplePalette(t, pal);
  }
  DataFrame edgesDF = DataFrame::create(
    _["x"]=x, _["y"]=y, _["z"]=z,
    _["x_end"]=x_end, _["y_end"]=y_end, _["z_end"]=z_end,
    _["width"]=width, _["color"]=color,
    _["stringsAsFactors"]=false
  );

  // ---- nodes: carry geometry + parsed metadata for labeling/coloring in R ----
  int NN = static_cast<int>(nodes.size());
  NumericVector nx(NN), ny(NN), nz(NN), nrad(NN), x_off(NN), z_off(NN), lo(NN), hi(NN);
  IntegerVector nid(NN), step(NN);
  LogicalVector incl_lo(NN), incl_hi(NN);
  CharacterVector item(NN), feature(NN), kind(NN), category_val(NN), lbl(NN), lbl_short(NN);

  for (int i=0;i<NN;++i) {
    nx[i]   = nodes[i].x; ny[i]   = nodes[i].y; nz[i]   = nodes[i].z;
    x_off[i]= nodes[i].x_offset; z_off[i]= nodes[i].z_offset;
    nrad[i] = nodes[i].node_radius;
    nid[i]  = nodes[i].item; step[i] = nodes[i].step;

    item[i]       = id_to_item[nodes[i].item];
    feature[i]    = nodes[i].type;   // base feature name (already parsed)
    kind[i]       = nodes[i].kind;
    lo[i]         = nodes[i].interval_low;
    hi[i]         = nodes[i].interval_high;
    incl_lo[i]    = nodes[i].incl_low;
    incl_hi[i]    = nodes[i].incl_high;
    category_val[i] = nodes[i].category_val;
    lbl[i]        = nodes[i].interval_label;
    lbl_short[i]  = nodes[i].interval_label_short;
  }

  DataFrame nodesDF = DataFrame::create(
    _["x"]=nx, _["y"]=ny, _["z"]=nz,
    _["x_offset"]=x_off, _["z_offset"]=z_off,
    _["radius"]=nrad,
    _["id"]=nid, _["item"]=item,
    _["step"]=step,
    _["feature"]=feature,
    _["kind"]=kind,
    _["interval_low"]=lo, _["interval_high"]=hi,
    _["incl_low"]=incl_lo, _["incl_high"]=incl_hi,
    _["category_val"]=category_val,
    _["interval_label"]=lbl,
    _["interval_label_short"]=lbl_short
  );

  return List::create(
    _["edges"]       = edgesDF,
    _["nodes"]       = nodesDF,
    _["edge_metric"] = edge_metric,
    _["edge_range"]  = NumericVector::create(minV, maxV)
  );
}
