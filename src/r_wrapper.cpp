#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <tuple>
#include <limits>
#include <cmath>

#include "coral_plots.h"
#include "coral_layout_builder.h"
#include "string_splitter.h"

using namespace Rcpp;

/// @brief Retrieves the integer ID for a given item string, adding it to the map and registry if not already present.
///
/// If the item exists in the lookup map, its ID is returned. Otherwise, a new ID is generated,
/// the item is added to both the lookup map and registry vector, and the new ID is returned.
///
/// @param lookup A map from item strings to integer IDs.
/// @param registry A vector maintaining the order and names of items (ID to string mapping).
/// @param item The item string to look up or insert.
/// @return The integer ID corresponding to the given item.
static int get_item_id(std::unordered_map<std::string,int>& lookup,
                       std::vector<std::string>& registry,
                       const std::string& item) {
  auto it = lookup.find(item);
  if (it != lookup.end()) return it->second;
  int id = static_cast<int>(registry.size());
  lookup.emplace(item, id);
  registry.push_back(item);
  return id;
}

static std::vector<int> to_std_vec(const IntegerVector& v) {
  std::vector<int> out; out.reserve(v.size());
  for (int x : v) out.push_back(x);
  return out;
}

//' @title Entry point for R to generate coral plot data from a set of association rules.
//'
//' @description This function takes a list of parsed association rules and produces two data frames:
//' one for the nodes and one for the edges of a coral plot. It acts as a wrapper that:
//' \itemize{
//'    \item Constructs the coral layout.
//'    \item Converts the resulting nodes and edges into R-compatible data frames.
//' }
//'
//' @param parsed TODO
//' @param grid_size Number of grid cells per layout dimension to use during node positioning.
//' @param lhs_sort The sorting metric for tzhe antecedent
//'
//' @return A List containing two DataFrames:
//' \itemize{
//'    \item \code{edges}: DataFrame with start and end coordinates, line width, and color for each edge.
//'    \item \code{nodes}: DataFrame with position, radius, ID, and item label for each node.
//' }
//' @keywords internal
//' @export
// [[Rcpp::export]]
Rcpp::List build_layout_cpp(Rcpp::List parsed,
                            int grid_size,
                            std::string lhs_sort = "confidence") {
  
  // ---- unpack parsed ----
  Rcpp::DataFrame items = Rcpp::as<Rcpp::DataFrame>(parsed["items"]);
  Rcpp::DataFrame rules = Rcpp::as<Rcpp::DataFrame>(parsed["rules"]);
  IntegerVector item_id = items["item_id"];
  CharacterVector item_label = items["label"];

  const int M = items.nrow();

  // id_to_item seeded with the existing atomic items (0-based)
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

  // metric selector for LHS sorting
  int metric_to_use = 0; // 0=confidence, 1=support, 2=lift
  if (lhs_sort == "support") metric_to_use = 1;
  else if (lhs_sort == "lift") metric_to_use = 2;

  // run layout
  std::vector<coral_plots::Edge> edges;
  std::vector<coral_plots::Node> nodes;

  coral_plots::CoralLayoutBuilder::build(
    cpp_rules, grid_size, 0.4, id_to_item, nodes, edges, metric_to_use
  );

  int E = static_cast<int>(edges.size());

  NumericVector x(E), y(E), z(E), x_end(E), y_end(E), z_end(E), support(E), lift(E), confidence(E);
  for (int i=0;i<E;++i) {
    x[i]     = edges[i].x_start;
    y[i]     = edges[i].y_start;
    z[i]     = edges[i].z_start;
    x_end[i] = edges[i].x_end;
    y_end[i] = edges[i].y_end;
    z_end[i] = edges[i].z_end;
	support[i] = edges[i].support;
	lift[i] = edges[i].lift;
	confidence[i] = edges[i].confidence;
  }
  DataFrame edgesDF = DataFrame::create(
    _["x"]=x, _["y"]=y, _["z"]=z,
    _["x_end"]=x_end, _["y_end"]=y_end, _["z_end"]=z_end,
    _["support"]=support, _["lift"]=lift, _["confidence"]=confidence,
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
    _["nodes"]       = nodesDF
  );
}
