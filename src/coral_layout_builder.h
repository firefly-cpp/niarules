#ifndef CORAL_LAYOUT_BUILDER_H
#define CORAL_LAYOUT_BUILDER_H
#include <map>
#include <vector>

#include "coral_plots.h"

namespace coral_plots {
    class CoralLayoutBuilder {
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
        /// @brief Holds the metrics for a single association rule.
        ///
        /// Contains the confidence, lift, and support values that quantify
        /// the strength and usefulness of the rule.
        struct RuleMetric {
            double confidence; ///< Confidence value of the rule.
            double lift; ///< Lift value of the rule.
            double support; ///< Support value of the rule.
        };

        /// @brief Represents a path of rules with aggregated metrics.
        ///
        /// Contains the identifier for the rule path and average confidence,
        /// lift, and support metrics calculated across all rules in the path.
        struct Path {
            RulePath path_id; ///< Identifier for the rule path.
            double confidence; ///< Average confidence of the rules in the path.
            double lift; ///< Average lift of the rules in the path.
            double support; ///< Average support of the rules in the path.
        };

        /// @brief Groups rules by their unique rule ID.
        ///
        /// Iterates through a list of rules and inserts each rule into an unordered map,
        /// using the rule's `rule_id` as the key. This enables quick lookup of rules by ID.
        ///
        /// @param rules A vector of Rule objects to be grouped.
        /// @return An unordered map associating each rule ID with its corresponding Rule.
        static std::unordered_map<int, Rule> groupRulesById(const std::vector<Rule> &rules);

        /// @brief Groups single-antecedent rules by their consequent item ID.
        ///
        /// Filters the input rules to include only those with a single-item antecedent,
        /// then groups these into a map keyed by the consequent ID. Each group contains
        /// `SingleMetric` entries derived from the rule's confidence, lift, and support.
        ///
        /// @param rules A vector of Rule objects to be analyzed.
        /// @return An unordered map where each key is a consequent ID and each value is a vector of associated SingleMetric objects.
        static std::unordered_map<int, std::vector<SingleMetric> > groupRulesBySingleMetrics(const std::vector<Rule> &rules);

        /// @brief Groups rules by their consequent item ID.
        ///
        /// Organizes the input rules into a map where each key is a consequent item ID,
        /// and the corresponding value is a list of rule IDs that have that consequent.
        ///
        /// @param rules A vector of Rule objects to be grouped.
        /// @return An unordered map from consequent item IDs to vectors of rule IDs.
        static std::unordered_map<int, std::vector<int> > groupRulesByConsequent(const std::vector<Rule> &rules);

        /// @brief Sorts a list of item IDs based on a selected metric or alphabetically as a fallback.
        ///
        /// Retrieves `SingleMetric` values associated with a given consequent. If metrics are available,
        /// it scores each item based on the selected metric (0 = confidence, 1 = support, 2 = lift) and
        /// sorts them in descending order of that metric. If no metrics are found, it falls back to
        /// alphabetical sorting based on `id_to_item`.
        ///
        /// @param consequent The ID of the consequent item used to look up relevant metrics.
        /// @param items A vector of item IDs to sort.
        /// @param id_to_item A mapping from item IDs to their string names, used for fallback sorting.
        /// @param single_metrics A map of consequent IDs to vectors of SingleMetric objects.
        /// @param metric_to_use An integer specifying which metric to sort by: 0 = confidence, 1 = support, 2 = lift.
        /// @return A sorted vector of item IDs based on the selected metric or alphabetically.
        static std::vector<int> sortByMetric(
            int consequent,
            std::vector<int> items,
            const std::vector<std::string> &id_to_item,
            std::unordered_map<int, std::vector<SingleMetric> > single_metrics,
            int metric_to_use = 0
        );

        /// @brief Groups path structures by consequent item ID, aggregating metrics across shared rule paths.
        ///
        /// For each consequent, generates all incremental rule paths that begin with the consequent and are extended
        /// by the sorted antecedents of associated rules. Each unique path accumulates the confidence, lift, and support
        /// metrics of all rules that contribute to it. The final output maps each consequent to a list of `Path` objects
        /// representing these aggregated rule paths.
        ///
        /// @param rules_by_id A map of rule IDs to their corresponding Rule objects.
        /// @param rules_by_consequent A map of consequent item IDs to lists of rule IDs.
        /// @param id_to_item A mapping from item IDs to string names, used for sorting.
        /// @param single_metrics A map of consequent IDs to vectors of SingleMetric objects, used for sorting antecedents.
        /// @return An unordered map from consequent item IDs to vectors of aggregated Path objects.
        static std::unordered_map<int, std::vector<Path> > groupPathsByConsequent(
            std::unordered_map<int, Rule> &rules_by_id,
            const std::unordered_map<int, std::vector<int> > &rules_by_consequent,
            const std::vector<std::string> &id_to_item,
            const std::unordered_map<int, std::vector<SingleMetric> > &single_metrics
        );

        /// @brief Indexes Path objects by their path ID.
        ///
        /// Constructs a map from each `Path` object's `path_id` to the corresponding `Path`,
        /// enabling efficient lookup of metrics by unique rule path identifiers.
        ///
        /// @param paths A vector of Path objects to be indexed.
        /// @return A map from RulePath identifiers to their corresponding Path objects.
        static std::map<RulePath, Path> groupMetricsByPathID(std::vector<Path> const &paths);

        /// @brief Builds a map of parent-to-child rule paths based on path structure.
        ///
        /// For each path in the input, identifies its parent by removing the last element
        /// and maps the parent to its corresponding child path. This defines a tree-like
        /// hierarchy of rule paths for traversal or visualization.
        ///
        /// @param metrics_by_path_id A map of RulePath to Path objects, used to derive parent-child relationships.
        /// @return A map where each key is a parent RulePath and the value is a list of its child RulePaths.
        static std::map<RulePath, std::vector<RulePath> > calculateChildren(const std::map<RulePath, Path> &metrics_by_path_id);

        /// @brief Computes the number of leaf paths under each rule path node in the hierarchy.
        ///
        /// Performs a depth-first traversal over the rule path tree starting from a root (defined by the consequent ID),
        /// counting how many leaf paths (i.e., paths with no children) descend from each node. The result can be used
        /// for layout sizing, importance scoring, or pruning.
        ///
        /// @param rhs The consequent item ID used as the root of the rule path tree.
        /// @param children A map from each parent RulePath to its child RulePaths.
        /// @param metrics_by_path_id A map from RulePath to Path objects (not directly used in traversal logic but ensures all nodes are visited).
        /// @return A pair consisting of the root RulePath and a map from RulePaths to their corresponding leaf counts.
        static std::pair<RulePath, std::map<RulePath, int> > calculateLeafCounts(
            int rhs,
            std::map<RulePath, std::vector<RulePath> > children,
            const std::map<RulePath, Path> &metrics_by_path_id
        );

        /// @brief Extracts support and lift values from path metrics into separate lookup maps.
        ///
        /// Iterates over all paths and collects their support and lift values into two distinct maps,
        /// keyed by the RulePath. These maps allow for quick access to each metric independently.
        ///
        /// @param metrics_by_path_id A map from RulePath to Path objects containing the metrics.
        /// @return A pair of maps: the first maps RulePath to support, and the second maps RulePath to lift.

        static std::pair<std::map<RulePath, double>, std::map<RulePath, double> > getSupportAndLiftNodes(const std::map<RulePath, Path> &metrics_by_path_id);

        /// @brief Determines the maximum depth among all rule paths.
        ///
        /// Calculates the length of the longest RulePath in the provided map, which corresponds
        /// to the deepest level in the rule path hierarchy.
        ///
        /// @param metrics_by_path_id A map from RulePath to Path objects.
        /// @return The maximum depth (i.e., the largest RulePath length).
        static int determineMaxDepth(const std::map<RulePath, Path> &metrics_by_path_id);

        /// @brief Computes the angular spans (start and end angles) for each node in a radial layout.
        ///
        /// Traverses the rule path hierarchy level by level and assigns each node an angular range
        /// based on its relative number of descendant leaves. This enables the construction of
        /// radial visualizations where space is proportionally divided by subtree size.
        ///
        /// @param root The root RulePath (usually containing only the consequent ID).
        /// @param max_depth The maximum depth of the rule path tree.
        /// @param children A map from parent RulePaths to their child RulePaths.
        /// @param leaf_counts A map from RulePaths to the number of leaf nodes in their subtrees.
        /// @param metrics_by_path_id A map of RulePaths to Path objects (used to determine levels).
        /// @return A pair of maps: the first contains the start angles, the second the end angles for each RulePath.
        static std::pair<std::map<RulePath, double>, std::map<RulePath, double> > computeAngularSpans(
            const RulePath &root,
            int max_depth,
            std::map<RulePath, std::vector<RulePath> > children,
            std::map<RulePath, int> leaf_counts,
            const std::map<RulePath, Path> &metrics_by_path_id
        );

        /// @brief Builds node objects for a radial layout and computes their 3D coordinates.
        ///
        /// For each RulePath, computes angular and radial positions based on its depth, angle span,
        /// and associated lift metric. Node positions are calculated in 3D space (x, y, z),
        /// and their visual size (node radius) is scaled according to lift. All nodes are stored
        /// in the provided `all_nodes` vector, and their coordinates are returned as a lookup map.
        ///
        /// @param x_off X-axis offset for positioning the root.
        /// @param z_off Z-axis offset for positioning the root.
        /// @param support_node A map of RulePaths to their support values.
        /// @param lift_node A map of RulePaths to their lift values.
        /// @param root The root RulePath (typically the consequent item only).
        /// @param max_depth Maximum depth of the rule path hierarchy.
        /// @param max_radius The maximum radial distance from the root node.
        /// @param leaf_counts A map from RulePaths to their number of descendant leaf paths.
        /// @param metrics_by_path_id A map from RulePaths to Path objects, used to drive node creation.
        /// @param a_start A map of angular start positions for each RulePath.
        /// @param a_end A map of angular end positions for each RulePath.
        /// @param all_nodes Output vector to which all constructed nodes will be appended.
        /// @return A map from RulePath to 3D coordinates (x, y, z) of each node.
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
            std::vector<Node> &all_nodes,
            const std::vector<int>& root_item_ids
        );

        /// @brief Constructs edges between nodes based on rule paths and their metrics.
        ///
        /// For each Path, creates an Edge connecting its parent and child nodes, assigning
        /// edge properties such as support, confidence, lift, and visual line width scaled
        /// by support values. The start and end coordinates are taken from the provided node coordinate map.
        ///
        /// @param paths A vector of Path objects representing rule paths.
        /// @param coordinates A map from RulePath to 3D coordinates of nodes.
        /// @param all_edges Output vector to which all constructed edges will be appended.
        static void buildEdges(const std::vector<Path> &paths, std::map<RulePath, std::tuple<double, double, double> > coordinates, std::vector<Edge> &all_edges);
    };
}


#endif //CORAL_LAYOUT_BUILDER_H
