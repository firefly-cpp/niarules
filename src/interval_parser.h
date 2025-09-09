#ifndef INTERVAL_PARSER
#define INTERVAL_PARSER

#include "string_splitter.h"

#include <sstream>
#include <algorithm>
#include <cctype>
#include <string>
#include <vector>
#include <limits>
#include <cmath>

static inline std::string base_feature_name(const std::string& x) {
    // strip up to first bracket, operator, TOP-LEVEL comma, or " in " / "%in%"
    size_t n = x.size();
    int d_par = 0, d_brk = 0, d_brc = 0;

    auto is_op = [](char c) { return c == '(' || c == '[' || c == '<' || c == '>' || c == '='; };

    size_t i = 0;
    for (; i < n; ++i) {
        char c = x[i];

        // track depths so we only react to top-level commas
        if (c == '(') ++d_par;
        else if (c == ')') d_par = std::max(0, d_par - 1);
        else if (c == '[') ++d_brk;
        else if (c == ']') d_brk = std::max(0, d_brk - 1);
        else if (c == '{') ++d_brc;
        else if (c == '}') d_brc = std::max(0, d_brc - 1);

        // stop on any operator
        if (is_op(c)) break;

        // stop on TOP-LEVEL comma
        if (c == ',' && d_par == 0 && d_brk == 0 && d_brc == 0) break;

        // crude " in " / "%in%" handling: bail on space then 'i' or '%'
        if (c == ' ' && i + 1 < n && (x[i + 1] == 'i' || x[i + 1] == '%')) break;
    }

    std::string s = trim_copy(x.substr(0, i));
    if (!s.empty() && s.back() == ' ') s.pop_back();
    return s;
}

struct ParsedItem {
    std::string kind;           // "numeric" | "categorical" | "unknown"
    std::string type;           // base feature name
    double low = std::numeric_limits<double>::quiet_NaN();
    double high = std::numeric_limits<double>::quiet_NaN();
    bool incl_low = false;
    bool incl_high = false;
    std::string category_val;
    std::string interval_label;
    std::string interval_label_short;
    std::string rel_op;
};

static inline std::string fmt_num(double v, int digits = 3) {
    if (std::isnan(v)) return "NA";
    if (!std::isfinite(v)) return v > 0 ? "\u221E" : "-\u221E";
    std::ostringstream oss;
    oss.setf(std::ios::fixed);
    oss.precision(digits);
    oss << v;
    std::string s = oss.str();
    // trim trailing zeros and dot
    while (!s.empty() && s.back() == '0') s.pop_back();
    if (!s.empty() && s.back() == '.') s.pop_back();
    return s;
}

// very close to your R logic
static ParsedItem parse_interval_info(std::string item_label) {
    ParsedItem out;
    item_label = trim_copy(item_label);

    // Early out if multiple top-level items
    auto many = split_outside_brackets(item_label);
    if (many.size() > 1u) {
        // Use ONLY the first token to determine a reasonable base "type"
        std::string first = trim_copy(many[0]);
        out.kind = "unknown";
        out.type = base_feature_name(first);
        out.interval_label = item_label;         // keep full for display if you want
        out.interval_label_short = first;        // short label from first token
        return out;
    }

    const std::string feat = base_feature_name(item_label);

    // bracketed interval:  (..., ...) or [..., ...]
    {
        size_t open = std::string::npos, close = std::string::npos;
        char open_chr = 0, close_chr = 0;
        for (size_t i = 0; i < item_label.size(); ++i) {
            if (item_label[i] == '(' || item_label[i] == '[') { open = i; open_chr = item_label[i]; break; }
        }
        if (open != std::string::npos) {
          // accept EITHER kind of closing bracket for mixed intervals ([lo,hi) or (lo,hi])
            for (size_t i = item_label.size(); i-- > 0; ) {
                if (item_label[i] == ']' || item_label[i] == ')') { close = i; close_chr = item_label[i]; break; }
            }
            if (close != std::string::npos && close > open) {
                auto inside = item_label.substr(open + 1, close - open - 1);
                // split first top-level comma inside
                auto parts = split_outside_brackets(inside); // depth-aware; will only split top-level
                if (parts.size() == 2u) {
                    try {
                        double lo = std::stod(trim_copy(parts[0]));
                        double hi = std::stod(trim_copy(parts[1]));
                        out.kind = "numeric"; out.type = feat;
                        out.low = lo; out.high = hi;
                        out.incl_low = (open_chr == '[');
                        out.incl_high = (close_chr == ']');
                        out.interval_label = feat + " " + item_label.substr(open, close - open + 1);
                        out.interval_label_short =
                            out.type + " " + (out.incl_low ? "[" : "(") + fmt_num(out.low) + ", " + fmt_num(out.high) + (out.incl_high ? "]" : ")");
                        out.rel_op = "=";
                        return out;
                    }
                    catch (...) {}
                }
            }
        }
    }

    // relational:  Feature <= v, >= v, < v, > v, = v
    {
        // super simple token scan
        const std::string ops[] = { "<=" , ">=", "<", ">", "=" };
        for (auto op : ops) {
            auto pos = item_label.find(op);
            if (pos != std::string::npos) {
                auto left = trim_copy(item_label.substr(0, pos));
                auto right = trim_copy(item_label.substr(pos + op.size()));
                if (op == "=") {
                    out.kind = "categorical"; out.type = left;
                    out.category_val = right;
                    out.interval_label = left + " = " + right;
                    out.interval_label_short = out.interval_label;
                    out.rel_op = "in";
                    return out;
                }
                else {
                    try {
                        double v = std::stod(right);
                        out.kind = "numeric"; out.type = left;
                        if (op == "<" || op == "<=") {
                            out.low = -std::numeric_limits<double>::infinity();
                            out.high = v; out.incl_low = false; out.incl_high = (op == "<=");
                        }
                        else {
                            out.low = v; out.high = std::numeric_limits<double>::infinity();
                            out.incl_low = (op == ">="); out.incl_high = false;
                        }
                        out.interval_label = left + " " + op + " " + right;
                        out.interval_label_short = out.interval_label;
                        out.rel_op = "in";
                        return out;
                    }
                    catch (...) {}
                }
            }
        }
    }

    // set: Feature in {A,B} or %in%
    {
        auto pos_in = item_label.find(" in ");
        auto pos_pct = item_label.find("%in%");
        if (pos_in != std::string::npos || pos_pct != std::string::npos) {
            size_t pos = (pos_in != std::string::npos) ? pos_in : pos_pct;
            auto left = trim_copy(item_label.substr(0, pos));
            auto rest = trim_copy(item_label.substr(pos + 4)); // both tokens are length 4
            // If RHS starts with '[' or '(', treat as numeric interval (no extra braces)
            if (!rest.empty() && (rest.front() == '[' || rest.front() == '(')) {
            // Recurse locally by faking a label "<left> <rest>"
                auto fake = left + " " + rest;
                return parse_interval_info(fake);
            }
            // Otherwise expect {...} and keep set-notation
            auto vals = strip_outer_braces(rest);
            out.kind = "categorical"; out.type = left;
            out.category_val = vals; // raw
            out.interval_label = left + " in {" + vals + "}";
            out.interval_label_short = out.interval_label;
            out.rel_op = "in";
            return out;
        }
    }

    // fallback
    out.kind = "unknown"; out.type = feat;
    out.interval_label = item_label;
    out.interval_label_short = item_label;
    out.rel_op = "";
    return out;
}

#endif
