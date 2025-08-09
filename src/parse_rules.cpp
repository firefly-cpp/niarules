// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <limits>
#include <cctype>
#include <algorithm>

#include "string_splitter.h"   // for trim_copy
#include "interval_parser.h"   // for parse_interval_info / ParsedItem

using namespace Rcpp;

static std::string detect_op_from_label(const std::string& s) {
    if (s.find(">=") != std::string::npos) return ">=";
    if (s.find("<=") != std::string::npos) return "<=";
    if (s.find('>') != std::string::npos) return ">";
    if (s.find('<') != std::string::npos) return "<";
    if (s.find('=') != std::string::npos) return "=";
    if (s.find(" in ") != std::string::npos) return "in";
    return "";
}

static std::string unquote_if_quoted(const std::string& s) {
    if (s.size() >= 2 && s.front() == '"' && s.back() == '"')
        return s.substr(1, s.size() - 2);
    return s;
}

static bool parse_interval_bounds_from_label(const std::string& label, double& lo, double& hi) {
    // best-effort for scientific notation if the inner parser missed it
    auto lpos = label.find_first_of("[(");
    auto rpos = label.find_last_of("])");
    if (lpos == std::string::npos || rpos == std::string::npos || rpos <= lpos + 1) return false;
    std::string inner = label.substr(lpos + 1, rpos - lpos - 1);
    // split by comma
    size_t comma = inner.find(',');
    if (comma == std::string::npos) return false;
    std::string a = trim_copy(inner.substr(0, comma));
    std::string b = trim_copy(inner.substr(comma + 1));
    try {
        lo = std::stod(a);
        hi = std::stod(b);
        return true;
    }
    catch (...) { return false; }
}


// small splitter: split on ',' OR '&' that are OUTSIDE (), [] or {}
static std::vector<std::string> split_outside_brackets_comma_amp(std::string s) {
    std::vector<std::string> parts;
    std::string buf;
    int d_mix = 0;  // combines '(' + '['  and decrements on ')' + ']'
    int d_brc = 0;  // '{' / '}'

    auto flush = [&]() {
        std::string t = trim_copy(buf);
        if (!t.empty()) parts.push_back(t);
        buf.clear();
        };

    for (size_t i = 0; i < s.size(); ++i) {
        char ch = s[i];
        switch (ch) {
        case '(': case '[': ++d_mix; break;
        case ')': case ']': d_mix = std::max(0, d_mix - 1); break;
        case '{': ++d_brc; break;
        case '}': d_brc = std::max(0, d_brc - 1); break;
        default: break;
        }
        const bool top = (d_mix == 0 && d_brc == 0);
        if (top && (ch == ',' || ch == '&')) {
            flush();
        }
        else {
            buf.push_back(ch);
        }
    }
    flush();
    return parts;
}


// simple registry for item IDs (by exact trimmed string)
static int get_item_id(std::unordered_map<std::string, int>& lookup,
    std::vector<std::string>& registry,
    const std::string& item) {
    auto it = lookup.find(item);
    if (it != lookup.end()) return it->second;
    int id = static_cast<int>(registry.size());
    lookup[item] = id;
    registry.push_back(item);
    return id;
}

//' @export
// [[Rcpp::export]]
Rcpp::List parse_rules_cpp(Rcpp::DataFrame rules_df) {
    
    // required columns
    CharacterVector Antecedent = rules_df["Antecedent"];
    CharacterVector Consequence = rules_df["Consequence"];
    NumericVector Support = rules_df["Support"];
    NumericVector Confidence = rules_df["Confidence"];
    NumericVector Fitness = rules_df["Fitness"]; // used as 'lift' by convention

    const int n = Antecedent.size();
    if (Consequence.size() != n || Support.size() != n ||
        Confidence.size() != n || Fitness.size() != n) {
        stop("parse_rules_cpp: inconsistent column lengths.");
    }

    // global registries
    std::unordered_map<std::string, int> item_to_id;
    std::vector<std::string> id_to_item;

    // per-item parsed metadata (parallel to id_to_item; fill lazily)
    std::vector<std::string> item_feature;       // ParsedItem::type
    std::vector<std::string> item_kind;          // "numeric" | "categorical" | "set" | "unknown"
    std::vector<std::string> item_category_val;  // for categorical equality
    std::vector<double>      item_lo, item_hi;   // numeric bounds
    std::vector<int>         item_incl_lo, item_incl_hi; // logical
    std::vector<std::string> item_op;            // "<", "<=", ">", ">=", or ""
    std::vector<std::string> item_label_long;    // rich label
    std::vector<std::string> item_label_short;   // short label

    auto ensure_item_parsed = [&](int item_id) {
        // already filled?
        if (item_id < (int)item_feature.size() && !item_kind[item_id].empty()) return;

        // resize vectors if first time seeing beyond current size
        if (item_id >= (int)item_feature.size()) {
            const int new_size = (int)id_to_item.size();
            item_feature.resize(new_size);
            item_kind.resize(new_size);
            item_category_val.resize(new_size);
            item_lo.resize(new_size);
            item_hi.resize(new_size);
            item_incl_lo.resize(new_size);
            item_incl_hi.resize(new_size);
            item_op.resize(new_size);
            item_label_long.resize(new_size);
            item_label_short.resize(new_size);
        }

        const std::string& label = id_to_item[item_id];
        ParsedItem p = parse_interval_info(label); // trim-only tolerant

        item_feature[item_id] = p.type;
        item_kind[item_id] = p.kind;
        item_category_val[item_id] = unquote_if_quoted(p.category_val);

        // use label as source of truth, fall back to p.rel_op
        std::string op = detect_op_from_label(label);
        if (op.empty()) op = p.rel_op;
        item_op[item_id] = op;


        // numeric bounds: normal path
        double lo = p.low, hi = p.high;

        // relational items: set the unused bound to NA
        if (op == ">" || op == ">=") {
            hi = std::numeric_limits<double>::quiet_NaN();
        }
        else if (op == "<" || op == "<=") {
            lo = std::numeric_limits<double>::quiet_NaN();
        }
        else if (op == "=") {
            // categorical equality: no numeric bounds
            lo = hi = std::numeric_limits<double>::quiet_NaN();
        }
        else if (op == "in") {
            // if the inner parser failed on scientific notation, try to recover
            if (!(std::isfinite(lo) && std::isfinite(hi))) {
                double lo2, hi2;
                if (parse_interval_bounds_from_label(label, lo2, hi2)) { lo = lo2; hi = hi2; }
            }
        }

        item_lo[item_id] = lo;
        item_hi[item_id] = hi;
        item_incl_lo[item_id] = p.incl_low ? 1 : 0;
        item_incl_hi[item_id] = p.incl_high ? 1 : 0;
        item_label_long[item_id] = p.interval_label;
        item_label_short[item_id] = p.interval_label_short;
        };

    // build rule-wise LHS/RHS item-id vectors
    Rcpp::List lhs_ids(n), rhs_ids(n);
    IntegerVector rule_id(n);
    NumericVector out_support(n), out_confidence(n), out_lift(n);
    IntegerVector antecedent_length(n), consequent_length(n);

    for (int i = 0; i < n; ++i) {
        // LHS: split by ',' or '&' (outside brackets); no canonicalization beyond trimming
        std::vector<int> lhs_vec;
        {
            std::string lhs = Rcpp::as<std::string>(Antecedent[i]);
            auto parts = split_outside_brackets_comma_amp(lhs);
            lhs_vec.reserve(parts.size());
            for (const auto& tok : parts) {
                if (tok.empty()) continue;
                int id = get_item_id(item_to_id, id_to_item, tok);
                ensure_item_parsed(id);
                lhs_vec.push_back(id);
            }
        }

        // RHS: strip outer {...} once (if present), then split by ',' (and '&') ---
        std::vector<int> rhs_vec;
        {
            std::string rhs = Rcpp::as<std::string>(Consequence[i]);
            // trim
            rhs = trim_copy(rhs);
            if (rhs.size() >= 2 && rhs.front() == '{' && rhs.back() == '}') {
                rhs = rhs.substr(1, rhs.size() - 2);
            }
            auto parts = split_outside_brackets_comma_amp(rhs);
            rhs_vec.reserve(parts.size());
            for (const auto& tok : parts) {
                if (tok.empty()) continue;
                int id = get_item_id(item_to_id, id_to_item, tok);
                ensure_item_parsed(id);
                rhs_vec.push_back(id);
            }
        }

        // metrics
        rule_id[i] = i + 1; // 1-based for display
        out_support[i] = Support[i];
        out_confidence[i] = Confidence[i];
        out_lift[i] = Fitness[i]; // niarules' "Fitness" treated as lift
        antecedent_length[i] = (int)lhs_vec.size();
        consequent_length[i] = (int)rhs_vec.size();

        lhs_ids[i] = IntegerVector(lhs_vec.begin(), lhs_vec.end());
        rhs_ids[i] = IntegerVector(rhs_vec.begin(), rhs_vec.end());
    }

    lhs_ids.attr("class") = "AsIs";
    rhs_ids.attr("class") = "AsIs";

    // build items data.frame (0-based item_id by design)
    const int M = (int)id_to_item.size();
    IntegerVector item_id(M);
    CharacterVector label(M), feature(M), kind(M), category_value(M), op(M), label_long(M), label_short(M);
    NumericVector lo(M), hi(M);
    LogicalVector incl_low(M), incl_high(M);

    for (int j = 0; j < M; ++j) {
        // ensure parsed (in case an item appeared only on RHS/LHS)
        ensure_item_parsed(j);
        item_id[j] = j;
        label[j] = id_to_item[j];
        feature[j] = item_feature[j];
        kind[j] = item_kind[j];
        category_value[j] = item_category_val[j];
        lo[j] = item_lo[j];
        hi[j] = item_hi[j];
        incl_low[j] = (bool)item_incl_lo[j];
        incl_high[j] = (bool)item_incl_hi[j];
        op[j] = item_op[j];
        label_long[j] = item_label_long[j];
        label_short[j] = item_label_short[j];
    }

    DataFrame items = DataFrame::create(
        _["item_id"] = item_id,
        _["label"] = label,
        _["feature"] = feature,
        _["kind"] = kind,
        _["category_value"] = category_value,
        _["lo"] = lo,
        _["hi"] = hi,
        _["incl_low"] = incl_low,
        _["incl_high"] = incl_high,
        _["op"] = op,
        _["label_long"] = label_long,
        _["label_short"] = label_short,
        _["stringsAsFactors"] = false
    );

    // rules df with list-columns for lhs/rhs item IDs (0-based)
    DataFrame rules = DataFrame::create(
        _["rule_id"] = rule_id,
        _["support"] = out_support,
        _["confidence"] = out_confidence,
        _["lift"] = out_lift,
        _["lhs_item_ids"] = lhs_ids,
        _["rhs_item_ids"] = rhs_ids,
        _["antecedent_length"] = antecedent_length,
        _["consequent_length"] = consequent_length,
        _["stringsAsFactors"] = false
    );

    return List::create(
        _["items"] = items,
        _["rules"] = rules
    );
}
