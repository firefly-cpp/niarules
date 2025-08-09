// string_splitter.h
#ifndef STRING_SPLITTER
#define STRING_SPLITTER

#include <algorithm>
#include <cctype>
#include <string>
#include <vector>

static inline std::string trim_copy(std::string s) {
    auto not_space = [](unsigned char ch) { return !std::isspace(ch); };
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), not_space));
    s.erase(std::find_if(s.rbegin(), s.rend(), not_space).base(), s.end());
    return s;
}

// remove a single outer pair of { ... } if present
static inline std::string strip_outer_braces(std::string s) {
    s = trim_copy(s);
    if (s.size() >= 2 && s.front() == '{' && s.back() == '}') {
        return s.substr(1, s.size() - 2);
    }
    return s;
}

// split by commas that are OUTSIDE (), [], {}
static inline std::vector<std::string> split_outside_brackets(std::string s) {
    s = strip_outer_braces(s);
    std::vector<std::string> parts;
    std::string buf;
    int d_par = 0, d_brk = 0, d_brc = 0;

    auto flush = [&]() {
        auto t = trim_copy(buf);
        if (!t.empty()) parts.push_back(std::move(t));
        buf.clear();
        };

    for (char ch : s) {
        switch (ch) {
        case '(': ++d_par; break;
        case ')': if (d_par > 0) --d_par; break;
        case '[': ++d_brk; break;
        case ']': if (d_brk > 0) --d_brk; break;
        case '{': ++d_brc; break;
        case '}': if (d_brc > 0) --d_brc; break;
        default: break;
        }
        if (ch == ',' && d_par == 0 && d_brk == 0 && d_brc == 0) {
            flush();
        }
        else {
            buf.push_back(ch);
        }
    }
    flush();
    return parts;
}

#endif
