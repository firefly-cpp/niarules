#include <Rcpp.h>

using namespace Rcpp;

#include "coral_plots.h"

/// @brief Retrieves the integer ID for a given item string, adding it to the map and registry if not already present.
///
/// If the item exists in the lookup map, its ID is returned. Otherwise, a new ID is generated,
/// the item is added to both the lookup map and registry vector, and the new ID is returned.
///
/// @param lookup A map from item strings to integer IDs.
/// @param registry A vector maintaining the order and names of items (ID to string mapping).
/// @param item The item string to look up or insert.
/// @return The integer ID corresponding to the given item.
static int get_item_id(
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
    const DataFrame &df,
    std::unordered_map<std::string, int> &item_to_id,
    std::vector<std::string> &id_to_item
) {
    // names of the cols = hardcoded
    IntegerVector ids = df["rule_id"];
    CharacterVector rhs_chr = df["rhs"];
    NumericVector supports = df["support"];
    NumericVector confidences = df["confidence"];
    NumericVector lifts = df["lift"];
    IntegerVector lengths = df["antecedent_length"];

    const size_t n = ids.size();
    std::vector<coral_plots::Rule> out;
    out.reserve(n);

    for (size_t i = 0; i < n; ++i) {
        // map rhs/consequent string to ID
        const int rhs_id = get_item_id(item_to_id, id_to_item,
                                       static_cast<std::string>(rhs_chr[i]));

        // get the lhs/antecedent cols, convert strings to IDs
        const int k = lengths[i];
        std::vector<int> ant;
        ant.reserve(k);
        for (int j = 1; j <= k; ++j) {
            std::string col = "lhs_" + std::to_string(j);
            CharacterVector lhs_col = df[col];
            int lhs_id = get_item_id(item_to_id, id_to_item,
                                     static_cast<std::string>(lhs_col[i]));
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
List buildCoralPlots(const DataFrame &rulesDF, int grid_size) {
    std::unordered_map<std::string, int> item_to_id;
    std::vector<std::string> id_to_item;

    // convert wide data.frame into vector<Rule>
    std::vector<coral_plots::Rule> rules = df_to_rules(rulesDF, item_to_id, id_to_item);

    // call the layout algorithm
    std::vector<coral_plots::Edge> edges;
    std::vector<coral_plots::Node> nodes;
    coral_plots::buildCoralPlots(rules, edges, nodes, item_to_id, id_to_item, grid_size);

    // convert edges back to R
    int E = edges.size();
    NumericVector x_start(E), y_start(E), z_start(E), x_end(E), y_end(E), z_end(E), width(E);
    CharacterVector color(E, "green");
    for (int i = 0; i < E; ++i) {
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
    for (int i = 0; i < N; ++i) {
        radius[i] = nodes[i].node_radius;
        x[i] = nodes[i].x;
        y[i] = nodes[i].y;
        z[i] = nodes[i].z;
        x_offset[i] = nodes[i].x_offset;
        z_offset[i] = nodes[i].z_offset;
        id[i] = nodes[i].item;
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
