#include <Rcpp.h>

using namespace Rcpp;

#include "radial_layout.hpp"

static int get_item_id(std::unordered_map<std::string, int> &lookup,
    std::vector<std::string> &registry,
    std::string const& item)
{
    auto it = lookup.find(item);
    if (it != lookup.end())
        return it->second;
    int new_id = static_cast<int>(registry.size());
    lookup[item] = new_id;
    registry.push_back(item);
    return new_id;
}

// convert a "wide" R data.frame of rules into std::vector<Rule>
static std::vector<Rule> df_to_rules(
    const DataFrame& df,
    std::unordered_map<std::string,int>& item_to_id,
    std::vector<std::string>& id_to_item
)
{
	// names of the cols = hardcoded
    Rcpp::IntegerVector ids        = df["rule_id"];
    Rcpp::CharacterVector rhs_chr  = df["rhs"];
    Rcpp::NumericVector supports   = df["support"];
    Rcpp::NumericVector confidences= df["confidence"];
    Rcpp::NumericVector lifts      = df["lift"];
    Rcpp::IntegerVector lengths    = df["antecedent_length"];

    size_t n = ids.size();
    std::vector<Rule> out;
    out.reserve(n);

    for (size_t i = 0; i < n; ++i)
	{
        // map rhs/consequent string to ID
        int rhs_id = get_item_id(item_to_id, id_to_item,
            static_cast<std::string>(rhs_chr[i]));

        // get the lhs/antecendent cols, convert strings to IDs
        int k = lengths[i];
        std::vector<int> ant;
        ant.reserve(k);
        for (int j = 1; j <= k; ++j)
		{
            std::string col = "lhs_" + std::to_string(j);
            Rcpp::CharacterVector lhs_col = df[col];
            int lhs_id = get_item_id(item_to_id, id_to_item,
                static_cast<std::string>(lhs_col[i]));
            ant.push_back(lhs_id);
        }

        Rule r;
        r.rule_id     = ids[i];
        r.consequent  = rhs_id;
        r.antecedent  = std::move(ant);
        r.support     = supports[i];
        r.confidence  = confidences[i];
        r.lift        = lifts[i];
        out.push_back(std::move(r));
    }
    return out;
}

//' @export
// [[Rcpp::export]]
List wrapped_buildRadialPlots(const DataFrame& rulesDF, int grid_size)
{
	std::unordered_map<std::string, int> item_to_id;
	std::vector<std::string> id_to_item;
	
    // convert wide data.frame into vector<Rule>
    std::vector<Rule> rules = df_to_rules(rulesDF, item_to_id, id_to_item);

    // call the layout algorithm
    std::vector<Edge> edges;
    std::vector<Node> nodes;
    buildRadialPlots(rules, edges, nodes, item_to_id, id_to_item, grid_size);

    // convert edges back to R
    int E = edges.size();
    NumericVector x_start(E), y_start(E), z_start(E), x_end(E), y_end(E), z_end(E), width(E);
	CharacterVector color(E, "green");
    for(int i = 0; i < E; ++i)
	{
        x_start[i] = edges[i].x_start;
        y_start[i] = edges[i].y_start;
        z_start[i] = edges[i].z_start;
		x_end[i] = edges[i].x_end;
        y_end[i] = edges[i].y_end;
        z_end[i] = edges[i].z_end;
		width[i] = edges[i].line_width;
    }
    DataFrame edgesDF = DataFrame::create(
        _["x"] = x_start,
        _["y"] = y_start,
        _["z"] = z_start,
		_["x_end"] = x_end,
        _["y_end"] = y_end,
        _["z_end"] = z_end,
		_["width"] = width,
		_["color"] = color
    );

    // convert nodes back to R
    int N = nodes.size();
    NumericVector x(N), y(N), z(N), radius(N), x_offset(N), z_offset(N);
	IntegerVector id(N);
    CharacterVector item(N);
    for(int i = 0; i < N; ++i)
	{
        radius[i] = nodes[i].node_radius;
        x[i] = nodes[i].x;
        y[i] = nodes[i].y;
        z[i] = nodes[i].z;
		x_offset[i] = nodes[i].x_offset;
		z_offset[i] = nodes[i].z_offset;
        id[i]  = nodes[i].item;
        item[i] = id_to_item[nodes[i].item];
    }
    DataFrame nodesDF = DataFrame::create(
        _["x"] = x,
        _["y"] = y,
        _["z"] = z,
		_["radius"] = radius,
		_["id"] = id,
        _["item"] = item
    );

	// and done
    return List::create(
        _["edges"] = edgesDF,
        _["nodes"] = nodesDF
    );
}
