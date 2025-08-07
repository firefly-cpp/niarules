#include "coral_plots.h"

#include <Rcpp.h>
#include <chrono>

#include "coral_layout_builder.h"

namespace coral_plots {
    void buildCoralPlots(
        const std::vector<Rule> &rules,
        std::vector<Edge> &edges,
        std::vector<Node> &nodes,
        std::unordered_map<std::string, int> &item_to_id,
        const std::vector<std::string> &id_to_item,
        const int grid_size
    ) {
        Rcpp::Rcout << "number of rules " << rules.size() << "\n";
        Rcpp::Rcout << "requested grid size " << grid_size << "\n";

        const auto t0 = std::chrono::high_resolution_clock::now();

        CoralLayoutBuilder::build(rules, grid_size, 0.5, id_to_item, nodes, edges);

        const auto t1 = std::chrono::high_resolution_clock::now();
        const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0);
        Rcpp::Rcout << "calculated layouts - elapsed time: " << dt.count() << " milliseconds\n";
    }
}
