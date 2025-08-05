#ifndef RADIALLAYOUTBUILDER_H
#define RADIALLAYOUTBUILDER_H
#include <map>
#include <vector>

#include "radial_layout.h"


class RadialLayoutBuilder {
public:
    /// @brief Constructs a radial layout for given association rules.
    ///
    /// This function processes a set of association rules to build a layout of nodes and edges suitable for radial visualization.
    /// It organizes the rules by their consequents, computes metrics, sorts antecedent items based on rule metrics,
    /// and then assigns spatial positions and sizes to nodes and edges in a radial coordinate system.
    ///
    /// The layout places each consequent in a grid cell and arranges the antecedents in concentric circles around it, with angular spans proportional to the number of leaves (paths) underneath.
    /// Node radius and edge width are scaled based on lift and support metrics, respectively.
    ///
    /// @param rules Vector of Rule objects representing association rules.
    /// @param grid_size The size of the grid to lay out multiple consequents.
    /// @param max_radius Maximum radius to use for node placement in radial layout.
    /// @param id_to_item Vector mapping item IDs to their string names.
    /// @param[out] all_nodes Vector to be filled with constructed nodes representing rules and items.
    /// @param[out] all_edges Vector to be filled with constructed edges representing rule connections.
    static void build(
        const std::vector<Rule> &rules,
        int grid_size,
        double max_radius,
        const std::vector<std::string> &id_to_item,
        std::vector<Node> &all_nodes,
        std::vector<Edge> &all_edges
    );

private:
    struct RuleMetric {
        double confidence, lift, support;
    };

    struct Path {
        RulePath path_id;
        double confidence, lift, support;
    };

    static std::unordered_map<int, Rule> groupRulesById(const std::vector<Rule> &rules);

    static std::unordered_map<int, std::vector<SingleMetric> > groupRulesBySingleMetrics(const std::vector<Rule> &rules);

    static std::unordered_map<int, std::vector<int> > groupRulesByConsequent(const std::vector<Rule> &rules);

    static std::vector<int> sortByMetric(
        int consequent,
        std::vector<int> items,
        const std::vector<std::string> &id_to_item,
        std::unordered_map<int, std::vector<SingleMetric> > single_metrics,
        int metric_to_use = 0
    );

    static std::unordered_map<int, std::vector<Path> > groupPathsByConsequent(
        std::unordered_map<int, Rule> &rules_by_id,
        const std::unordered_map<int, std::vector<int> > &rules_by_consequent,
        const std::vector<std::string> &id_to_item,
        const std::unordered_map<int, std::vector<SingleMetric> > &single_metrics
    );

    static std::map<RulePath, Path> groupMetricsByPathID(std::vector<Path> const &paths);

    static std::map<RulePath, std::vector<RulePath> > calculateChildren(const std::map<RulePath, Path> &metrics_by_path_id);

    static std::pair<RulePath, std::map<RulePath, int> > calculateLeafCounts(
        int rhs,
        std::map<RulePath, std::vector<RulePath> > children,
        const std::map<RulePath, Path> &metrics_by_path_id
    );

    static std::pair<std::map<RulePath, double>, std::map<RulePath, double> > getSupportAndLiftNodes(const std::map<RulePath, Path> &metrics_by_path_id);

    static int determineMaxDepth(const std::map<RulePath, Path> &metrics_by_path_id);

    static std::pair<std::map<RulePath, double>, std::map<RulePath, double> > computeAngularSpans(
        const RulePath &root,
        int max_depth,
        std::map<RulePath, std::vector<RulePath> > children,
        std::map<RulePath, int> leaf_counts,
        const std::map<RulePath, Path> &metrics_by_path_id
    );

    static std::map<RulePath, std::tuple<double, double, double> > buildNodes(
        double x_off,
        double z_off,
        std::map<RulePath, double> support_node,
        std::map<RulePath, double> lift_node,
        const RulePath &root,
        int max_depth,
        double max_radius,
        std::map<RulePath, int> leaf_counts,
        const std::map<RulePath, Path> &metrics_by_path_id,
        std::map<RulePath, double> a_start,
        std::map<RulePath, double> a_end,
        std::vector<Node> &all_nodes
    );

    static void buildEdges(const std::vector<Path> &paths, std::map<RulePath, std::tuple<double, double, double> > coordinates, std::vector<Edge> &all_edges);
};


#endif //RADIALLAYOUTBUILDER_H
