/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Some sorting fun. The functions here are arbitrarily limited, don't expect
 * all the features you may need to be implemented
 */
#ifndef _SORT_H_
#define _SORT_H_

#include <vector>
#include <algorithm>
#include <utility>

using std::vector;
using std::swap;
using std::pair;

/* Sorts an array so that all elements smaller than mid occur before all
 * elements that are equal to mid, and all elements equal to mid occur before
 * all elements that are larger to mid.
 *
 * Returns the index of the first mid element and the size of the mid strike. If
 * there are no mid or larger than mid elements, it returns (vector.size(), 0)
 */
template <typename T>
pair<int, int> dutch_flag(vector<T>* values, const T& mid, size_t start,
                          int size) {
  int lo = start;
  int hi = start + size - 1;

  int i = start;
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
  return {lo, hi - lo + 1};
}

template <typename T>
pair<int, int> dutch_flag(vector<T>* values, const T& mid) {
  return dutch_flag(values, mid, 0, values -> size());
}

/* Sorts the segment of `values` of size `size`, since (and including)
 * `start`. Sorting goes from low to high.
 *
 * Note that this function does not shuffle the array before sorting, if your
 * order is not random you may fall pray of quadratic times (i.e. shuffle it
 * yourself first.
 *
 * This uses the 3-way merging function described above, so it is particularly
 * efficient to sort vectors with many repeated values.
 */
template <typename T>
void quicksort(vector<T>* values, size_t start,  int size) {
  if (size <= 1) {
    return;
  }

  int pivot = (*values)[start];
  pair<int, int> mid_range = dutch_flag(values, pivot, start, size);
  quicksort(values, start, mid_range.first - start);
  quicksort(values, mid_range.first + mid_range.second,
            size - mid_range.second - (mid_range.first - start));
}

template <typename T>
void quicksort(vector<T>* values) {
  quicksort(values, 0, values -> size());
}

#endif
