#pragma once

#include <string>
#include <vector>
#include <unordered_map>

struct Rule
{
    int                  rule_id;
    int                  consequent;
    std::vector<int>     antecedent;
    double               support;
    double               confidence;
    double               lift;
};

struct SingleMetric
{
    int    item;            // lhs item id
    double confidence;
    double lift;
    double support;
};

typedef std::vector<int> RulePath;

struct Node
{
    // TODO replace by unique ids
    RulePath   path_id;
    unsigned   step;            // = path_id.size()
    // TODO turn item into label (string)
    int        item;            // = path_id.back()

    int        leafcount;
    double     support_node;        // avg support of outgoing edges
    double     lift_node;           // avg lift of incoming edges

    double     angle_start, angle_end;
    double     angle, radius;
    double     x_offset, z_offset;  // grid?cell center
    double     x, y, z;             // final coords (y=0 in this layout)
    double     node_radius;         // mapped from lift_node
};

struct Edge
{
    // TODO replace by unique ids
    RulePath   parent_path, child_path;
    double     support, confidence, lift;
    double     x_start, y_start, z_start;
    double     x_end, y_end, z_end;
    double     line_width;
    int        lift_bin;
};

void buildRadialPlots(
	std::vector<Rule> rules, std::vector<Edge> & edges,
	std::vector<Node> &nodes,
	std::unordered_map<std::string, int> &item_to_id,
	std::vector<std::string> &id_to_item,
	int grid_size
	);