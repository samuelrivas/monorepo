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

string print_pair(pair<int, int> p) {
  ostringstream oss;
  oss << "(" << p.first << "," << p.second << ")";
  return oss.str();
}

bool sort_comp(const pair<int, int>& a, const pair<int, int>& b) {
  return a.first < b.first;
}

// pass by copy/move since we need to sort this
bool conflict(vector<pair<int, int>> events) {
  sort(events.begin(), events.end(), sort_comp);

  for (size_t i = 1; i < events.size(); i++) {
    if (events[i].first <= events[i - 1].second) {
      cerr << print_pair(events[i - 1])
           << " clashes with "
           << print_pair(events[i])
           << endl;
      return true;
    }
  }
  return false;
}

int main(void) {
  // start time and end time, both included
  vector<vector<pair<int, int>>> tests {
    {{11, 14}, {17, 17}, {0, 3}, {9, 11}, {5, 5}},
    {{12, 14}, {17, 17}, {0, 3}, {9, 11}, {5, 5}}
  };

  cout << "This should be 1: " << conflict(tests[0]) << endl;
  cout << "This should be 0: " << conflict(tests[1]) << endl;
}
