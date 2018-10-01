// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <vector>
#include <iostream>
#include <algorithm>
#include <iomanip>

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::swap;
using std::setw;

template <typename T>
void dutch_flag(vector<T>* values, const T& mid) {
  int lo = 0;
  int hi = values -> size() - 1;

  int i = 0;
  while (i <= hi) {
    int value = (*values)[i];

    if (value < mid) {
      swap((*values)[lo], (*values)[i]);
      lo++;
      i++;
    } else if (value > mid) {
      swap((*values)[hi], (*values)[i]);
      hi--;
    } else {
      i++;
    }
  }
}

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
    {3, 3, 3, 1, 3, 1}
  };

  for (auto test : tests) {
    cout << "Test  : ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl;

    dutch_flag<int>(&test, 2);
    cout << "Result: ";
    for (int i : test) {
      cout << setw(3) << i;
    }
    cout << endl << endl;
  }
  return 0;
}
