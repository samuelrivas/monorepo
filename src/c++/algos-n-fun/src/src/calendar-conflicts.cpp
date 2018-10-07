// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <iomanip>
#include <vector>
#include <utility>
#include <algorithm>
#include <string>
#include <sstream>

using std::cout;
using std::cerr;
using std::endl;
using std::setw;
using std::vector;
using std::pair;
using std::sort;
using std::string;
using std::ostringstream;
using std::max;

string print_pair(pair<int, int> p) {
  ostringstream oss;
  oss << "(" << p.first << "," << p.second << ")";
  return oss.str();
}

bool sort_comp(const pair<int, int>& a, const pair<int, int>& b) {
  return a.first < b.first;
}

// pass by copy/move since we need to sort this
vector<vector<pair<int, int>>> find_conflicts(vector<pair<int, int>> events) {
  if (events.size() == 0) {
    return {};
  }

  sort(events.begin(), events.end(), sort_comp);

  vector<vector<pair<int, int>>> conflicts;
  vector<pair<int, int>> temp_conflicts { events[0] };
  int latest_end = events[0].second;

  for (size_t i = 1; i < events.size(); i++) {
    if (events[i].first > latest_end) {
      // No conflict
      if (temp_conflicts.size() > 1) {
        conflicts.push_back(temp_conflicts);
        cerr << "closing a conflict of size " << temp_conflicts.size() << endl;
      }

      temp_conflicts.clear();
    }
    temp_conflicts.push_back(events[i]);
    latest_end = max(latest_end, events[i].second);
  }

  if (temp_conflicts.size() > 1) {
    cerr << "closing a conflict of size " << temp_conflicts.size() << endl;
    conflicts.push_back(temp_conflicts);
  }

  return conflicts;
}

int main(void) {
  // start time and end time, both included
  vector<vector<pair<int, int>>> tests {
    {{11, 14}, {17, 17}, {0, 3}, {9, 11}, {5, 5}},
    {{12, 14}, {17, 17}, {0, 3}, {9, 11}, {5, 5}},
    {{1, 2}, {3, 5}, {4, 6}, {7, 12}, {8, 9}, {10, 12}, {13, 14}, {13, 14}}
  };

  for (auto test : tests) {
    cout << "Test ::" << endl;
    for (auto conflict : find_conflicts(test)) {
      for (auto event : conflict) {
        cout << print_pair(event) << " ";
      }
      cout << endl;
    }
  }
  // cout << "This should be 1: " << find_conflicts(tests[0]).size() << endl;
  // cout << "This should be 0: " << find_conflicts(tests[1]).size() << endl;
  // cout << "This should be 3: " << find_conflicts(tests[2]).size() << endl;
}
