// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <vector>
#include <iomanip>
#include <numeric>
#include <cassert>
#include <cmath>

using std::vector;
using std::cout;
using std::endl;
using std::setw;
using std::accumulate;

int find_pivot(const vector<int>& l) {
  assert(l.size() > 0);

  int sum = accumulate(l.begin(), l.end(), 0);

  int left = 0;
  int right = sum - l[0];
  size_t pos = 0;

  while (left < right && pos < l.size()) {
    left += l[pos];
    pos++;
    right -= l[pos];
  }

  if (pos == 0) {
    return 0;
  }

  int diff_now = abs(left - right);
  int diff_before = abs(left - l[pos - 1] - (right + l[pos]));

  return (diff_now < diff_before) ? pos : pos - 1;
}

int main(void) {
  vector<vector<int>> tests {
    {1, 2, 3},
    {4, 1, 1},
    {1, 1, 4},
    {10, 3, 7, 2, 9, 4, 1, 1, 3}
  };

  for (auto test : tests) {
    for (int n : test) {
      cout << setw(2) << n;
    }
    cout << " : " << find_pivot(test) << endl;
  }
}
