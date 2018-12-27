// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <string>
#include <vector>
#include <utility>
#include <cstdlib>

using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::pair;

bool check_edits(const string& w1, const string& w2,
                 size_t pos1, size_t pos2,
                 int edits_left) {
  if ((pos1 == w1.size() && pos2 + edits_left >= w2.size())
      || (pos2 == w2.size() && pos1 + edits_left >= w1.size())) {
    return true;
  }

  if (w1[pos1] == w2[pos2]) {
    return check_edits(w1, w2, pos1 + 1, pos2 + 1, edits_left);
  }

  if (edits_left <= 0) {
    return false;
  }

  return check_edits(w1, w2, pos1 + 1, pos2 + 1, edits_left - 1) // rewrite
    || check_edits(w1, w2, pos1 + 1, pos2, edits_left - 1) // skip/insert
    || check_edits(w1, w2, pos1, pos2 + 1, edits_left - 1); // skip/insert
}

bool check_edits(const string& w1, const string& w2) {
  if (abs(w1.size() - w2.size()) > 1) {
    return false;
  }

  bool edited {false};
  for (size_t i = 0, j = 0; i < w1.size();) {
    if (w1[i] != w2[j]) {
      if (edited) {
        return false;
      } else {
        edited = true;
        if (w1.size() >= w2.size()) {
          i++;
        }
        if (w2.size() >= w1.size()) {
          j++;
        }
      }
    } else {
      i++;
      j++;
    }
  }
  return true;
}

int main(void) {
  vector<pair<string, string>> tests {
    { "foo", "foo" },
    { "foo", "fooo" },
    { "foo", "foooo" },
    { "foo", "fooa" },
    { "fooa", "foo" },
    { "foo", "oo" },
    { "foo", "fao" },
    { "f", "" },
    { "foo", "fuu" },
    { "foo", "o" },
    { "foo", "bor" }
  };

  for (auto test: tests) {
    cout << test.first << " - " << test.second << " -> "
         << check_edits(test.first, test.second) << endl;
  }
  return 0;
}
