#include <vector>
#include <utility>
#include <iostream>
#include <algorithm>

using std::vector;
using std::pair;
using std::cout;
using std::endl;
using std::max;

vector<pair<int,int>> merge(const vector<pair<int,int>>& x,
                            const vector<pair<int,int>>& y) {
  size_t pos_x = 0;
  size_t pos_y = 0;

  bool open_interval = false;
  pair<int,int> next_interval;
  vector<pair<int,int>> result;

  while (pos_x < x.size() || pos_y < y.size()) {
    pair<int,int> candidate;
    if (pos_x < x.size() && pos_y < y.size()) {
      if (x[pos_x].first < y[pos_y].first) {
        candidate = x[pos_x++];
      } else {
        candidate = y[pos_y++];
      }
    } else if (pos_x < x.size()) {
      candidate = x[pos_x++];
    } else {
      candidate = y[pos_y++];
    }

    if (!open_interval) {
      next_interval = candidate;
      open_interval = true;
    } else if (candidate.first > next_interval.second) {
      result.push_back(next_interval);
      next_interval = candidate;
    } else {
      next_interval.second = max(next_interval.second, candidate.second);
    }
  }
  result.push_back(next_interval);
  return result;
}

int main(void) {
  vector<pair<int,int>> test_x = {{1, 3}, {5, 9}, {11, 13}, {15, 18}, {19, 22}};
  vector<pair<int,int>> test_y = {{2, 3}, {6, 10}, {12, 20}};

  for (pair<int, int> interval : merge(test_x, test_y)) {
    cout << "[" << interval.first
         << "," << interval.second
         << "]" << endl;
  }
  return 0;
}
