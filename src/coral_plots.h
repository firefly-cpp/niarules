#ifndef CORAL_PLOTS_H
#define CORAL_PLOTS_H

#include <string>
#include <unordered_map>
#include <vector>
#include <limits>

namespace coral_plots {
#define PI 3.14159265358979323846

    /// @brief Represents an association rule.
    ///
    /// A rule is defined by its antecedent items and a single consequent item.
    /// It also carries standard rule metrics: support, confidence, and lift.
    struct Rule {
        int rule_id; ///< Unique identifier for the rule.
        int consequent; ///< ID of the consequent item (right-hand side).
        std::vector<int> antecedent; ///< List of item IDs forming the antecedent (left-hand side).
        double support; ///< Support value of the rule.
        double confidence; ///< Confidence value of the rule.
        double lift; ///< Lift value of the rule.
    };

    /// @brief Represents a single-item rule metric.
    ///
    /// Used to evaluate individual items in the antecedent by extracting their associated confidence, support, and lift values from 1-item rules.
    struct SingleMetric {
        int item; ///< ID of the item (used in LHS of 1-item rule).
        double confidence; ///< Confidence of the rule where this item is the antecedent.
        double lift; ///< Lift of the rule where this item is the antecedent.
        double support; ///< Support of the rule where this item is the antecedent.
    };

    /// @brief A vector of item IDs representing a rule path.
    ///
    /// A RulePath is a sequence of item IDs that encodes a path in the rule tree.
    /// The first element is typically the consequent, followed by sorted antecedent items.
    typedef std::vector<int> RulePath;

    /// @brief Represents a node in the coral layout.
    ///
    /// Each node corresponds to a rule prefix (RulePath) and stores its visual and structural properties, including metrics for layout (angle, radius),
    /// and metrics for data-driving sizing (support, lift).
    struct Node {

        RulePath path_id; ///< Unique identifier of the node, given by its rule path.
        unsigned step; ///< Depth in the tree; equal to path_id.size().
        int item; ///< ID of the item represented by this node (last item in path).

        int leafcount; ///< Number of leaf nodes in the subtree rooted at this node.
        double support_node; ///< Average support of all outgoing edges from this node.
        double lift_node; ///< Average lift of all incoming edges to this node.

        double angle_start; ///< Starting angle for the node's angular span (in radians).
        double angle_end; ///< Ending angle for the node's angular span (in radians).
        double angle; ///< Center angle of the node’s angular span (in radians).
        double radius; ///< Coral distance from the center of the plot.

        double x_offset; ///< X-coordinate of the grid cell center this node belongs to.
        double z_offset; ///< Z-coordinate of the grid cell center this node belongs to.

        double x; ///< Final x-position after layout calculation.
        double y; ///< Final y-position (set to 0 in this layout).
        double z; ///< Final z-position after layout calculation.

        double node_radius; ///< Visual radius of the node, scaled by lift_node value.

        std::string type;
        std::string kind;
        double interval_low = std::numeric_limits<double>::quiet_NaN();
        double interval_high = std::numeric_limits<double>::quiet_NaN();
        bool incl_low = false, incl_high = false;
        std::string category_val;
        std::string interval_label;
        std::string interval_label_short;
    };

    /// @brief Represents an edge between two nodes in the coral layout.
    ///
    /// Edges represent relationships between parent and child rule paths.
    /// Each edge arries associated metrics and visual coordinates for rendering.
    struct Edge {
        RulePath parent_path; ///< Rule path of the parent node.
        RulePath child_path; ///< Rule path of the child node.

        double support; ///< Support value for the transition represented by this edge.
        double confidence; ///< Confidence value for the transition represented by this edge.
        double lift; ///< Lift value for the transition represented by this edge.

        double x_start; ///< X-coordinate of the start (parent) node.
        double y_start; ///< Y-coordinate of the start (parent) node.
        double z_start; ///< Z-coordinate of the start (parent) node.
        double x_end; ///< X-coordinate of the end (child) node.
        double y_end; ///< Y-coordinate of the end (child) node.
        double z_end; ///< Z-coordinate of the end (child) node.
    };
}

#endif //CORAL_PLOTS_H
