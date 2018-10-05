/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Some sorting fun
 */
#ifndef _SORT_H_
#define _SORT_H_

#include <vector>
#include <algorithm>

using std::vector;
using std::swap;

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

#endif
