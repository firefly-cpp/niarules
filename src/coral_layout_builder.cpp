#include "coral_layout_builder.h"

#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>

namespace coral_plots {
    void CoralLayoutBuilder::build(
        const std::vector<Rule> &rules,
        int grid_size,
        double max_radius,
        const std::vector<std::string> &id_to_item,
        std::vector<Node> &all_nodes,
        std::vector<Edge> &all_edges
    ) {
        auto rules_by_id = groupRulesById(rules);
        const auto rules_by_single_metrics = groupRulesBySingleMetrics(rules);
        const auto rules_by_consequent = groupRulesByConsequent(rules);
        auto paths_by_consequent = groupPathsByConsequent(rules_by_id, rules_by_consequent, id_to_item, rules_by_single_metrics);
        all_nodes.clear();
        all_edges.clear();
        unsigned plot_id = 0;
        for (const auto &[rhs, paths]: paths_by_consequent) {
            double x_off = plot_id / grid_size + 0.5;
            double z_off = plot_id % grid_size + 0.5;
            const auto metrics_by_path_id = groupMetricsByPathID(paths);
            auto children = calculateChildren(metrics_by_path_id);
            auto [root,leaf_counts] = calculateLeafCounts(rhs, children, metrics_by_path_id);
            auto [support_node, lift_node] = getSupportAndLiftNodes(metrics_by_path_id);
            // root’s outgoing support = average over children
            // root's lift = 0
            {
                auto const &ch = children[root];
                double sum = 0;
                for (auto const &child: ch) sum += support_node[child];
                support_node[root] = ch.empty() ? 0 : sum / ch.size();
                lift_node[root] = 0;
            }

            auto max_depth = determineMaxDepth(metrics_by_path_id);
            auto [a_start, a_end] = computeAngularSpans(root, max_depth, children, leaf_counts, metrics_by_path_id);
            auto coordinates = buildNodes(x_off, z_off, support_node, lift_node, root, max_depth, max_radius, leaf_counts, metrics_by_path_id, a_start, a_end, all_nodes);
            buildEdges(paths, coordinates, all_edges);

            ++plot_id;
        }
    }

    std::unordered_map<int, Rule> CoralLayoutBuilder::groupRulesById(const std::vector<Rule> &rules) {
        std::unordered_map<int, Rule> rules_by_id;
        for (auto const &rule: rules) {
            rules_by_id[rule.rule_id] = rule;
        }
        return rules_by_id;
    }

    std::unordered_map<int, std::vector<SingleMetric> > CoralLayoutBuilder::groupRulesBySingleMetrics(const std::vector<Rule> &rules) {
        std::unordered_map<int, std::vector<SingleMetric> > single_metrics;
        for (auto &rule: rules) {
            if (rule.antecedent.size() == 1)
                single_metrics[rule.consequent].push_back({rule.antecedent[0], rule.confidence, rule.lift, rule.support});
        }
        return single_metrics;
    }

    std::unordered_map<int, std::vector<int> > CoralLayoutBuilder::groupRulesByConsequent(const std::vector<Rule> &rules) {
        std::unordered_map<int, std::vector<int> > rules_by_consequent;
        for (auto &rule: rules) {
            rules_by_consequent[rule.consequent].push_back(rule.rule_id);
        }
        return rules_by_consequent;
    }

    std::vector<int> CoralLayoutBuilder::sortByMetric(
        const int consequent,
        std::vector<int> items,
        const std::vector<std::string> &id_to_item,
        std::unordered_map<int, std::vector<SingleMetric> > single_metrics,
        const int metric_to_use
    ) {
        // look up single metrics from the prev step
        const auto it = single_metrics.find(consequent);

        // fallback if not found: sort alphabetically (look up the strings in id_to_item)
        if (it == single_metrics.end()) {
            std::sort(items.begin(), items.end(), [&](int a, int b) { return id_to_item[a] < id_to_item[b]; });
            return items;
        }

        // build a score map
        std::unordered_map<int, double> score;
        for (auto &m: it->second)
            score[m.item] = metric_to_use == 2 ? m.lift : metric_to_use == 1 ? m.support : m.confidence;

        // default 0
        for (int id: items) if (!score.count(id)) score[id] = 0.0;
        // sort descending
        std::sort(items.begin(), items.end(), [&](int a, int b) {
            if (score[a] != score[b]) return score[a] > score[b];
            return id_to_item[a] < id_to_item[b];
        });
        return items;
    }

    std::unordered_map<int, std::vector<CoralLayoutBuilder::Path> >
    CoralLayoutBuilder::groupPathsByConsequent(
        std::unordered_map<int, Rule> &rules_by_id,
        const std::unordered_map<int, std::vector<int> > &rules_by_consequent,
        const std::vector<std::string> &id_to_item,
        const std::unordered_map<int, std::vector<SingleMetric> > &single_metrics
    ) {
        std::unordered_map<int, std::vector<Path> > paths_per_consequent;

        for (const auto &[rhs, rule_IDs]: rules_by_consequent) {
            std::map<RulePath, std::vector<RuleMetric> > accumulated_metrics;

            for (int rid: rule_IDs) {
                Rule const &rule = rules_by_id[rid];
                RuleMetric metric{rule.confidence, rule.lift, rule.support};

                // sort the lhs depending on metric (here -> confidence)
                auto sorted_lhs = sortByMetric(rhs, rule.antecedent, id_to_item, single_metrics, 0);

                RulePath prefix;
                prefix.reserve(1 + sorted_lhs.size());
                prefix.push_back(rhs);

                // now we build all sub-paths incrementally, and immediately add the rule metrics to the accumulated metrics array for later usage.
                // e.g. if we have rhs 101 and sorted lhs 1 2, we build
                // 101 1
                // 101 1 2
                // and add the rule metrics to each of those.
                // and if we have another rule with rhs 101 and lhs 1 3, then that rule will be added to
                // 101 1
                // 101 1 3
                // so paths that are shared by several rules (101 - 1 in this case) get the metrics of the shared rules
                for (int sorted_lh: sorted_lhs) {
                    prefix.push_back(sorted_lh);
                    accumulated_metrics[prefix].push_back(metric);
                }
            }

            auto &out = paths_per_consequent[rhs];
            out.reserve(accumulated_metrics.size());
            for (auto const &[rulePath, metrics]: accumulated_metrics) {
                Path p;
                p.path_id = rulePath;

                double sum_conf = 0, sum_lift = 0, sum_support = 0;
                for (auto const &m: metrics) {
                    sum_conf += m.confidence;
                    sum_lift += m.lift;
                    sum_support += m.support;
                }

                size_t N = metrics.size();
                p.confidence = (N ? sum_conf / N : 0.0);
                p.lift = (N ? sum_lift / N : 0.0);
                p.support = (N ? sum_support / N : 0.0);

                out.push_back(std::move(p));
            }
        }
        return paths_per_consequent;
    }

    std::map<RulePath, CoralLayoutBuilder::Path> CoralLayoutBuilder::groupMetricsByPathID(std::vector<Path> const &paths) {
        std::map<RulePath, Path> metrics_by_path_id;
        for (auto const &path: paths) metrics_by_path_id[path.path_id] = path;
        return metrics_by_path_id;
    }

    std::map<RulePath, std::vector<RulePath> > CoralLayoutBuilder::calculateChildren(const std::map<RulePath, Path> &metrics_by_path_id) {
        std::map<RulePath, std::vector<RulePath> > children;
        for (const auto &[child, _]: metrics_by_path_id) {
            RulePath parent(child.begin(), child.end() - 1);
            children[parent].push_back(child);
        }
        return children;
    }

    std::pair<RulePath, std::map<RulePath, int> > CoralLayoutBuilder::calculateLeafCounts(
        const int rhs, std::map<RulePath, std::vector<RulePath> > children,
        const std::map<RulePath, Path> &metrics_by_path_id
    ) {
        std::map<RulePath, int> leaf_count;
        std::function<int(RulePath const &)> dfs = [&](RulePath const &node) {
            if (const auto it = leaf_count.find(node); it != leaf_count.end()) return it->second;
            const auto cit = children.find(node);
            if (cit == children.end() || cit->second.empty())
                return leaf_count[node] = 1;
            int sum = 0;
            for (auto const &c: cit->second) sum += dfs(c);
            return leaf_count[node] = sum;
        };

        const RulePath root{rhs};
        dfs(root);
        for (const auto &[node, _]: metrics_by_path_id) dfs(node);
        return {root, leaf_count};
    }

    std::pair<std::map<RulePath, double>, std::map<RulePath, double> > CoralLayoutBuilder::getSupportAndLiftNodes(const std::map<RulePath, Path> &metrics_by_path_id) {
        std::map<RulePath, double> support_node, lift_node;
        for (const auto &[rhs, path]: metrics_by_path_id) {
            support_node[rhs] = path.support;
            lift_node[rhs] = path.lift;
        }
        return {support_node, lift_node};
    }

    int CoralLayoutBuilder::determineMaxDepth(const std::map<RulePath, Path> &metrics_by_path_id) {
        int max_depth = 1;
        for (const auto &[key, _]: metrics_by_path_id)
            max_depth = std::max(max_depth, static_cast<int>(key.size()));
        return max_depth;
    }

    std::pair<std::map<RulePath, double>, std::map<RulePath, double> > CoralLayoutBuilder::computeAngularSpans(
        const RulePath &root,
        const int max_depth,
        std::map<RulePath, std::vector<RulePath> > children,
        std::map<RulePath, int> leaf_counts,
        const std::map<RulePath, Path> &metrics_by_path_id
    ) {
        std::map<RulePath, double> a_start, a_end;
        a_start[root] = 0.0; // full circle at depth=1
        a_end[root] = 2 * PI;
        for (int d = 2; d <= max_depth; ++d) {
            // collect all prefixes of length d
            std::vector<RulePath> level;
            for (const auto &[key, _]: metrics_by_path_id)
                if (static_cast<int>(key.size()) == d)
                    level.push_back(key);

            for (auto const &node: level) {
                RulePath parent(node.begin(), node.end() - 1);
                auto siblings = children[parent];
                std::sort(siblings.begin(), siblings.end()); // deterministic

                // total leaves under this parent
                int total = 0;
                for (auto const &s: siblings) total += leaf_counts[s];

                // prefix sum of leaves before 'node'
                int acc = 0;
                for (auto const &s: siblings) {
                    if (s == node) break;
                    acc += leaf_counts[s];
                }

                const double a0 = a_start[parent];
                const double span = a_end[parent] - a0;
                a_start[node] = a0 + span * static_cast<double>(acc) / total;
                a_end[node] = a0 + span * static_cast<double>(acc + leaf_counts[node]) / total;
            }
        }
        return {a_start, a_end};
    }

    std::map<RulePath, std::tuple<double, double, double> > CoralLayoutBuilder::buildNodes(
        double x_off,
        double z_off,
        std::map<RulePath, double> support_node,
        std::map<RulePath, double> lift_node,
        const RulePath &root,
        const int max_depth,
        const double max_radius,
        std::map<RulePath, int> leaf_counts,
        const std::map<RulePath, Path> &metrics_by_path_id,
        std::map<RulePath, double> a_start,
        std::map<RulePath, double> a_end,
        std::vector<Node> &all_nodes
    ) {
        // find global lift range for node-radius mapping
        double min_l = 1e9, max_l = -1e9;
        for (const auto &[_, val]: lift_node) {
            min_l = std::min(min_l, val);
            max_l = std::max(max_l, val);
        }

        // build nodes, mapping lift to radius & also capture their coords in a map
        std::map<RulePath, std::tuple<double, double, double> > coordinates;
        coordinates[root] = std::make_tuple(x_off, 0.0, z_off);
        for (const auto &[pfx, _]: metrics_by_path_id) {
            constexpr double min_r = 0.005;
            constexpr double max_r = 0.02;
            Node node;
            node.path_id = pfx;
            node.step = static_cast<unsigned>(pfx.size());
            node.item = pfx.back();
            node.leafcount = leaf_counts[pfx];
            node.support_node = support_node[pfx];
            node.lift_node = lift_node[pfx];
            node.angle_start = a_start[pfx];
            node.angle_end = a_end[pfx];
            node.angle = 0.5 * (node.angle_start + node.angle_end);
            node.radius = (node.step - 1.0) / (max_depth - 1.0) * max_radius;
            node.x_offset = x_off;
            node.z_offset = z_off;
            node.x = node.x_offset + node.radius * std::cos(node.angle);
            node.z = node.z_offset + node.radius * std::sin(node.angle);
            node.y = 0.0;

            if (max_l > min_l)
                node.node_radius = (node.lift_node - min_l) / (max_l - min_l) * (max_r - min_r) + min_r;
            else
                node.node_radius = 0.5 * (min_r + max_r);

            coordinates[pfx] = {node.x, node.y, node.z};

            all_nodes.push_back(std::move(node));
        }
        return coordinates;
    }

    void CoralLayoutBuilder::buildEdges(const std::vector<Path> &paths, std::map<RulePath, std::tuple<double, double, double> > coordinates, std::vector<Edge> &all_edges) {
        double min_s = 1e9, max_s = -1e9;
        for (auto const &p: paths) {
            min_s = std::min(min_s, p.support);
            max_s = std::max(max_s, p.support);
        }

        for (auto const &p: paths) {
            Edge e;
            auto const &child = p.path_id;
            auto parent = RulePath(child.begin(), child.end() - 1);

            e.child_path = child;
            e.parent_path = parent;
            e.support = p.support;
            e.confidence = p.confidence;
            e.lift = p.lift;

            std::tie(e.x_start, e.y_start, e.z_start) = coordinates[parent];
            std::tie(e.x_end, e.y_end, e.z_end) = coordinates[child];

            // line width from [1..5]
            if (max_s > min_s)
                e.line_width = (p.support - min_s) / (max_s - min_s) * 4.0 + 1.0;
            else
                e.line_width = 3.0;

            all_edges.push_back(std::move(e));
        }
    }
}
