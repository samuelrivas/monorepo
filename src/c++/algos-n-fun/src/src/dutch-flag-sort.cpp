// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <vector>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include <utility>

#include "lib/sort.hpp"

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::swap;
using std::setw;
using std::pair;

int main(void) {
  vector<vector<int>> tests = {
    { 1, 1, 1, 1 },
    { 3, 3, 3, 3 },
    { 2, 2, 2, 2 },
    { 2, 1, 2, 3, 3, 1, 2 },
    { 1, 1, 3, 3, 3, 2, 2 },
    { 3, 3, 3, 2, 2, 1 },
    { 2, 2, 2, 3, 3, 1, 2 },
    { 3, 2, 3, 2 },
    { 2, 3, 2, 3 },
    { 3, 3, 3, 1, 3, 1 }
  };

  for (auto test : tests) {
    cout << "Test  : ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl;

    pair<int, int> range = dutch_flag<int>(&test, 2);
    cout << "Result: ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl;
    cout << "Range: (" << range.first << "," << range.second << ")"
         << endl << endl;
  }
  return 0;
}
