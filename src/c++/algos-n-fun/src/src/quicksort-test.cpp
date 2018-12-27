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
    { 3, 3, 3, 1, 3, 1 },
    { 9, 7, 10, 3, 6, 4, 0, 1, 8, 2, 5 },
    { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 },
    { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 },
    { 1, 1, 4, 0, 3, 4, 0, 2, 5, 5, 5, 1, 3, 5, 1, 5, 2, 0, 1, 0 }

  };

  for (auto test : tests) {
    cout << "Test  : ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl;

    quicksort<int>(&test);
    cout << "Result: ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl << endl;
  }
  return 0;
}
