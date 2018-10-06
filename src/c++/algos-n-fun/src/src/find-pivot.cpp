// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <vector>
#include <iomanip>
#include <numeric>
#include <cassert>
#include <cmath>

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::setw;
using std::accumulate;

int find_pivot(const vector<int>& l) {
  assert(l.size() > 0);

  int sum = accumulate(l.begin(), l.end(), 0);

  int left = 0;
  int right = sum - l[0];
  int best_pivot = 0;
  int best_diff = abs(right);

  for (size_t pivot = 1; pivot < l.size(); pivot++) {
    left += l[pivot - 1];
    right -= l[pivot];

    int diff = abs(left - right);
    if (diff < best_diff) {
      best_diff = diff;
      best_pivot = pivot;
    }
  }
  return best_pivot;
}

int main(void) {
  vector<vector<int>> tests {
    {1, 2, 3},
    {4, 1, 1},
    {1, 1, 4},
    {10, 3, 7, 2, 9, 4, 1, 1, 3},
    {10, 9, 0, -2, -3, -8, 3, -5, -2, 9, 3, -1, -2, 2, 7}
  };

  for (auto test : tests) {
    for (int n : test) {
      cout << setw(3) << n;
    }
    cout << " : " << find_pivot(test) << endl;
  }
}
