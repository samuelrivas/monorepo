/* Copyright 2017 <samuelrivas@gmail.com>
 *
 * From The Algorithm Design Manual:
 *
 * You have an unordered array X of n integers. Find the array M containing n
 * elements where each position M[i] the product of all the X numbers but
 * X[i]. You may not use division, but you can use extra memory.
 */

#include <cassert>
#include <iostream>
#include <vector>

using std::vector;
using std::cout;

void magic(const vector<int> &a, vector<int> *out) {
  int mult;
  size_t i;

  // Not well defined for arrays of less than two elements
  assert(a.size() > 1);

  for (i = 0, mult = 1; i < a.size(); i++) {
    (*out)[i] = mult;
    mult *= a[i];
  }
  for (i = a.size() - 1, mult = 1; i < a.size(); i--) {
    (*out)[i] *= mult;
    mult *= a[i];
  }
}

int main() {
  vector<int> a {10, 2, 3, 4, 5, 6};
  vector<int> out(a.size());

  magic(a, &out);

  for (int i : out) {
    cout << i << "\n";
  }
  return 0;
}
