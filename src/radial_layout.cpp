#include <Rcpp.h>

#include "radial_layout.hpp"

#include <cmath>
#include <algorithm>
#include <map>
#include <numeric>
#include <functional>
#include <chrono>

#define PI 3.14159265358979323846

void build_layout(std::vector<Rule> const& rules,
    int grid_size,
    double max_radius,
    const std::vector<std::string>& id_to_item,
    std::vector<Node> &all_nodes,
    std::vector<Edge> &all_edges)
{
    // build a rule lookup table
    std::unordered_map<int, Rule> rules_by_id;
    for (auto const& rule : rules)
    {
        rules_by_id[rule.rule_id] = rule;
    }

    // for each consequent, find all the 1-item rules leading to that consequent
    // -> needed to sort the longer rules
    // -> what do we do if rule set does not contain any rules of length 1?
    // basically, the mining step should make sure that those are contained
    std::unordered_map<int, std::vector<SingleMetric>> single_metrics;
    for (auto &rule : rules)
    {
        if (rule.antecedent.size() == 1)
            single_metrics[rule.consequent].push_back({ rule.antecedent[0], rule.confidence, rule.lift, rule.support });
    }

    // group the rules by consequent (could be merged w. prev step)
    std::unordered_map<int, std::vector<int>> rules_by_consequent;
    for (auto& rule : rules)
    {
        rules_by_consequent[rule.consequent].push_back(rule.rule_id);
    }

    // helper functions, for sorting the items that make up the lhs / antecedent based on single rule metrics
    // @last argument: 0 -> confidence, 1 -> support, 2 -> lift, add other shit as needed
    // items isn't a ref b/c I sort it
    auto sort_by_metric = [&](int consequent, std::vector<int> items, const int metric_to_use = 0)
        {
            // look up single metrics from prev step
            auto it = single_metrics.find(consequent);

            // fallback if not found: sort alphabetically (look up the strings in id_to_item)
            if (it == single_metrics.end())
            {
                std::sort(items.begin(), items.end(),
                    [&](int a, int b) { return id_to_item[a] < id_to_item[b]; });
                return items;
            }

            // build a score map
            std::unordered_map<int, double> score;
            for (auto& m : it->second)
                score[m.item] = (metric_to_use == 2 ? m.lift : metric_to_use == 1 ? m.support : m.confidence);

            // default 0
            for (int id : items) if (!score.count(id)) score[id] = 0.0;
            // sort descending
            std::sort(items.begin(), items.end(),
                [&](int a, int b) {
                    if (score[a] != score[b]) return score[a] > score[b];
                    return id_to_item[a] < id_to_item[b];
                });
            return items;
        };

    struct Path
    {
        RulePath path_id;              // we use the RulePath as unique identifier
        double confidence;             // aggregated
        double lift;                   // aggregated
        double support;                // aggregated
    };

    struct RuleMetric
    {
        double confidence;
        double lift;
        double support;
    };

    std::unordered_map<int, std::vector<Path>> paths_per_consequent;

    for (auto const& kv : rules_by_consequent)
    {
        int rhs = kv.first;
        std::vector<int> const& rule_ids = kv.second;
        std::map<RulePath, std::vector<RuleMetric>> accumulated_metrics;
        
        for (int rid : rule_ids)
        {
            Rule const& rule = rules_by_id[rid];
            RuleMetric metric{ rule.confidence, rule.lift, rule.support };

            // sort the lhs depending on metric (here -> confidence)
            auto sorted_lhs = sort_by_metric(rhs, rule.antecedent, 0);

            RulePath prefix;
            prefix.reserve(1 + sorted_lhs.size());
            prefix.push_back(rhs);

            // now we build all sub-paths incrementally, and immediately add the rule metrics to the accumulated metrics array for later usage
            // e.g. if we have rhs 101 and sorted lhs 1 2, we build
            // 101 1
            // 101 1 2
            // and add the rule metrics to each of those
            // and if we have another rule with rhs 101 and lhs 1 3, then that rule will be added to
            // 101 1
            // 101 1 3
            // so paths that are shared by several rules (101 - 1 in this case) get the metrics of the shared rules
            for (size_t j = 0; j < sorted_lhs.size(); ++j)
            {
                prefix.push_back(sorted_lhs[j]);
                accumulated_metrics[prefix].push_back(metric);
            }
        }

        // compute averages
        std::vector<Path> out;
        out.reserve(accumulated_metrics.size());
        for (auto const& kv2 : accumulated_metrics)
        {
            RulePath const& prefix = kv2.first;
            std::vector<RuleMetric> const& vec = kv2.second;

            double n = double(vec.size());
            double avg_c = std::accumulate(vec.begin(), vec.end(), 0.0,
                [](double s, RuleMetric const& m) {return s + m.confidence; }) / n;
            double avg_l = std::accumulate(vec.begin(), vec.end(), 0.0,
                [](double s, RuleMetric const& m) {return s + m.lift; }) / n;
            double avg_s = std::accumulate(vec.begin(), vec.end(), 0.0,
                [](double s, RuleMetric const& m) {return s + m.support; }) / n;
            out.push_back({ prefix, avg_c, avg_l, avg_s });
        }

        paths_per_consequent[rhs] = std::move(out);
    }

    all_nodes.clear();
    all_edges.clear();

// calc the layout

    unsigned plot_id = 0;
    for (auto const& kv : paths_per_consequent)
    {
        int rhs = kv.first;
        std::vector<Path> const& paths = kv.second;
        
        // gridcell for this rhs
        double x_off = (plot_id / grid_size) + 0.5;
        double z_off = (plot_id % grid_size) + 0.5;

        //std::cout << x_off << " " << z_off << std::endl;

        // build a lookup table, so that we can look up the metrics via the path id / prefix
        // (this looks a bit funny :D)
        std::map<RulePath, Path> metrics_by_path_id;
        for (auto const& path : paths) metrics_by_path_id[path.path_id] = path;

    // we need the leafcounts to calc the angular layout

        std::map<RulePath, std::vector<RulePath>> children;
        for (auto const& kv2 : metrics_by_path_id)
        {
            auto const& child = kv2.first;
            RulePath parent(child.begin(), child.end() - 1);
            children[parent].push_back(child);
        }

        // compute leafcounts via memoized DFS (this is straight from chagpt, pretty much the only thing I haven't checked yet)
        std::map<RulePath, int> leafcount;
        std::function<int(RulePath const&)> dfs = [&](RulePath const& node)
            {
            auto it = leafcount.find(node);
            if (it != leafcount.end()) return it->second;
            auto cit = children.find(node);
            if (cit == children.end() || cit->second.empty())
                return leafcount[node] = 1;
            int sum = 0;
            for (auto const& c : cit->second) sum += dfs(c);
            return leafcount[node] = sum;
            };

        RulePath root{ rhs };   // need this later
        dfs(root);
        for (auto const& kv2 : metrics_by_path_id) dfs(kv2.first);

        // per node support_node & lift_node
        std::map<RulePath, double> support_node, lift_node;
        for (auto const& kv2 : metrics_by_path_id)
        {
            support_node[kv2.first] = kv2.second.support;
            lift_node[kv2.first] = kv2.second.lift;
        }

        // root’s outgoing support = average over children
        // root's lift = 0
        {
            auto const& ch = children[root];
            double sum = 0;
            for (auto const& child : ch) sum += support_node[child];
            support_node[root] = ch.empty() ? 0 : sum / ch.size();
            lift_node[root] = 0;
        }

        // determine max depth for this consequent
        int max_depth = 1;
        for (auto const& kv2 : metrics_by_path_id)
            max_depth = std::max(max_depth, (int)kv2.first.size());

        // compute angular spans
        std::map<RulePath, double> a_start, a_end;
        a_start[root] = 0.0;      // full circle at depth=1
        a_end[root] = 2 * PI;
        for (int d = 2; d <= max_depth; ++d)
        {
            // collect all prefixes of length d
            std::vector<RulePath> level;
            for (auto const& kv2 : metrics_by_path_id)
                if ((int)kv2.first.size() == d)
                    level.push_back(kv2.first);

            for (auto const& node : level)
            {
                RulePath parent(node.begin(), node.end() - 1);
                auto sibs = children[parent];
                std::sort(sibs.begin(), sibs.end());  // deterministic

                // total leaves under this parent
                int total = 0;
                for (auto const& s : sibs) total += leafcount[s];

                // prefix sum of leaves before 'node'
                int acc = 0;
                for (auto const& s : sibs)
                {
                    if (s == node) break;
                    acc += leafcount[s];
                }

                double a0 = a_start[parent];
                double span = a_end[parent] - a0;
                a_start[node] = a0 + span * (double)acc / total;
                a_end[node] = a0 + span * (double)(acc + leafcount[node]) / total;
            }
        }

        // find global lift range for node-radius mapping
        double min_l = 1e9, max_l = -1e9;
        for (auto const& kv2 : lift_node)
        {
            min_l = std::min(min_l, kv2.second);
            max_l = std::max(max_l, kv2.second);
        }
        const double min_r = 0.005, max_r = 0.02;

        // build nodes, mapping lift to radius & also capture their coords in a map
        std::map<RulePath, std::tuple<double, double, double>> coords;
        coords[root] = std::make_tuple(x_off, 0.0, z_off);
        for (auto const& kv2 : metrics_by_path_id)
        {
            auto const& pfx = kv2.first;
            Node node;
            node.path_id = pfx;
            node.step = static_cast<unsigned>(pfx.size());
            node.item = pfx.back();
            node.leafcount = leafcount[pfx];
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

            coords[pfx] = { node.x, node.y, node.z };

            all_nodes.push_back(std::move(node));
        }

        // build edges, mapping support to line width
        double min_s = 1e9, max_s = -1e9;
        for (auto const& p : paths)
        {
            min_s = std::min(min_s, p.support);
            max_s = std::max(max_s, p.support);
        }

        for (auto const& p : paths)
        {
            Edge e;
            auto const& child = p.path_id;
            auto parent = RulePath(child.begin(), child.end() - 1);

            e.child_path = child;
            e.parent_path = parent;
            e.support = p.support;
            e.confidence = p.confidence;
            e.lift = p.lift;

            std::tie(e.x_start, e.y_start, e.z_start) = coords[parent];
            std::tie(e.x_end, e.y_end, e.z_end) = coords[child];

            // line width from [1..5]
            if (max_s > min_s)
                e.line_width = (p.support - min_s) / (max_s - min_s) * 4.0 + 1.0;
            else
                e.line_width = 3.0;

            all_edges.push_back(std::move(e));
        }

        ++plot_id;
    }
}

void buildRadialPlots(
	std::vector<Rule> rules,
	std::vector<Edge> &edges,
	std::vector<Node> &nodes,
	std::unordered_map<std::string, int> &item_to_id,
	std::vector<std::string> &id_to_item,
	int grid_size)
{
	std::cout << "number of rules " << rules.size() << std::endl;
	std::cout << "requested grid size " << grid_size << std::endl;;
	
	auto t0 = std::chrono::high_resolution_clock::now();

	build_layout(rules, grid_size, 0.5, id_to_item, nodes, edges);
	
	auto t1 = std::chrono::high_resolution_clock::now();
    auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0);
    std::cout << "calculated layouts - elapsed time: "  << dt.count() << " milliseconds\n";
	
	std::cout << "calculated node count " << nodes.size() << std::endl;
	std::cout << "calculated edge count " << edges.size() << std::endl;
}