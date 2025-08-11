#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <tuple>
#include <limits>
#include <cmath>

#include "coral_plots.h"
#include "coral_layout_builder.h"

using namespace Rcpp;

namespace {
	inline std::string path_to_string(const std::vector<int>& path, char sep='|') {
	  if (path.empty()) return {};
	  std::string out;
	  out.reserve(path.size() * 4); // rough prealloc
	  for (size_t i = 0; i < path.size(); ++i) {
		if (i) out.push_back(sep);
		out += std::to_string(path[i]);
	  }
	  return out;
	}

	inline std::vector<int> to_std_vec(const IntegerVector& v) {
	  std::vector<int> out; out.reserve(v.size());
	  for (int x : v) out.push_back(x);
	  return out;
	}
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

	std::unordered_map<int, std::vector<int>> rhs_components;

	// dedicated registry for composite RHS roots
	std::unordered_map<std::string,int> rhs_label_to_id;
	std::vector<std::string>            rhs_id_to_label;

	// helper for composite RHS root ids
	auto get_rhs_root_id = [&](const std::string& lab) -> int {
	  auto it = rhs_label_to_id.find(lab);
	  if (it != rhs_label_to_id.end()) return it->second;
	  int nid = static_cast<int>(rhs_id_to_label.size());
	  rhs_label_to_id.emplace(lab, nid);
	  rhs_id_to_label.push_back(lab);
	  return nid;
	};

	for (int i = 0; i < N; ++i) {
	  
	  // LHS
	  std::vector<int> lhs_vec;
	  if (lhs_ids_list[i] != R_NilValue) {
		lhs_vec = to_std_vec(Rcpp::IntegerVector(lhs_ids_list[i]));
	  }

	  // RHS items as ids
	  std::vector<int> rhs_vec;
	  if (rhs_ids_list[i] != R_NilValue) {
		rhs_vec = to_std_vec(Rcpp::IntegerVector(rhs_ids_list[i]));
	  }

	  // enforce non-empty RHS
	  if (rhs_vec.empty()) {
		Rcpp::stop("build_layout_cpp: encountered a rule with empty RHS; not supported.");
	  }

	  // build composite RHS label from atomic item labels (preserve order)
	  std::string rhs_label;
	  rhs_label.reserve(rhs_vec.size() * 8);
	  for (size_t k = 0; k < rhs_vec.size(); ++k) {
		int id = rhs_vec[k];
		if (id < 0 || id >= M) Rcpp::stop("build_layout_cpp: rhs item_id out of range.");
		if (k) rhs_label += ", ";
		rhs_label += id_to_item[id];
	  }

	  // assign a composite root id
	  int rhs_root_id = get_rhs_root_id(rhs_label);

	  // record components for this composite root
	  rhs_components[rhs_root_id] = rhs_vec;

	  // push rule
	  coral_plots::Rule r;
	  r.rule_id    = rule_id[i];
	  r.consequent = rhs_root_id;
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
    cpp_rules, grid_size, 0.4, id_to_item, nodes, edges, metric_to_use, rhs_components
  );

  int E = static_cast<int>(edges.size());

  NumericVector x(E), z(E), x_end(E), z_end(E), support(E), lift(E), confidence(E);
  CharacterVector parent_path(E), child_path(E);
  for (int i=0;i<E;++i) {
    x[i]     = edges[i].x_start;
    z[i]     = edges[i].z_start;
    x_end[i] = edges[i].x_end;
    z_end[i] = edges[i].z_end;
	support[i] = edges[i].support;
	lift[i] = edges[i].lift;
	confidence[i] = edges[i].confidence;
	parent_path[i] = path_to_string(edges[i].parent_path);
	child_path[i]  = path_to_string(edges[i].child_path);
  }
  DataFrame edgesDF = DataFrame::create(
    _["x"]=x, _["z"]=z,
    _["x_end"]=x_end, _["z_end"]=z_end,
    _["support"]=support, _["lift"]=lift, _["confidence"]=confidence,
	_["parent_path"] = parent_path,
	_["child_path"] = child_path
  );

  // ---- nodes: carry geometry + parsed metadata for labeling/coloring in R ----
  int NN = static_cast<int>(nodes.size());
  NumericVector nx(NN), nz(NN), nrad(NN), x_off(NN), z_off(NN), lo(NN), hi(NN);
  IntegerVector step(NN);
  LogicalVector incl_lo(NN), incl_hi(NN);
  CharacterVector item(NN), feature(NN), kind(NN), category_val(NN), lbl(NN), lbl_short(NN), path(NN);

  for (int i=0;i<NN;++i) {
    nx[i]   = nodes[i].x; nz[i]   = nodes[i].z;
    x_off[i]= nodes[i].x_offset; z_off[i]= nodes[i].z_offset;
    nrad[i] = nodes[i].node_radius;
    step[i] = nodes[i].step;
	path[i] = path_to_string(nodes[i].path_id);
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
    _["x"]=nx, _["z"]=nz,
    _["x_offset"]=x_off, _["z_offset"]=z_off,
    _["radius"]=nrad,
    _["item"]=item,
    _["step"]=step,
    _["feature"]=feature,
    _["kind"]=kind,
    _["interval_low"]=lo, _["interval_high"]=hi,
    _["incl_low"]=incl_lo, _["incl_high"]=incl_hi,
    _["category_val"]=category_val,
    _["interval_label"]=lbl,
    _["interval_label_short"]=lbl_short,
	_["path"] = path
  );

  return List::create(
    _["edges"]       = edgesDF,
    _["nodes"]       = nodesDF
  );
}
