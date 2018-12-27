// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <iomanip>
#include <vector>
#include <utility>
#include <algorithm>
#include <string>
#include <sstream>
#include <cassert>
#include <set>

using std::cout;
using std::cerr;
using std::endl;
using std::setw;
using std::vector;
using std::pair;
using std::sort;
using std::string;
using std::ostringstream;
using std::max;
using std::set;
using std::swap;

int factorial(int n) {
  int result = 1;
  for (int i = 2; i <=n; i++) {
    result *= i;
  }
  return result;
}

// Permutations are a vector with the most significant digit in position 0. The
// generator figures out the next permutation in numeric order. The relevant
// invariants are:
//
// - The position to increase is never the last one
// - Everything to the right of the position to increase is in descending order
// - We always increase by the minimum possible from the elements to the right
// - After increasing, everything to the right of the increased position is in
//   ascending order
//
// Just generate the sequence yourself to understand why those rules apply, and
// why we can just swap the elements when increasing and then reverse the right
// side to fulfill the last invariant
class Permutations {
  vector<int> current;
  int elements;
  bool first;

public:
  explicit Permutations(int _elements) :
    current(_elements),
    elements { _elements },
    first { true } {
    assert(_elements > 1);

    for (int i = 0; i < elements; i++) {
      current[i] = i + 1;
    }
  }
  vector<int> next() {
    // The first time we don't need to generate a new permutation
    if (first) {
      first = false;
      return current;
    }

    int pos_to_change = elements - 2;

    // Find a position where we can increase the value
    while (pos_to_change >= 0
           && current[pos_to_change] > current[pos_to_change + 1]) {
      pos_to_change--;
    }

    if (pos_to_change == 0
        && current[pos_to_change] > current[pos_to_change + 1]) {
      // Out of permutations
      assert(false);
    }

        // Find the smallest value to the right that is greater than pos_to_change
    int pos_to_swap = pos_to_change + 1;

    while (pos_to_swap < elements - 1
           && current[pos_to_swap + 1] > current[pos_to_change]) {
      pos_to_swap++;
    }

    // Swap them
    swap(current[pos_to_change], current[pos_to_swap]);

    // reverse all that is to the side of pos_to_change
    for (size_t i = pos_to_change + 1, j = elements - 1; i < j; i++, j--) {
      swap(current[i], current[j]);
    }

    return current;
  }

  bool has_next() {
    for (size_t i = 1; i < current.size(); i++) {
      if (current[i - 1] < current[i]) {
        return true;
      }
    }
    return false;
  }
};

int main(void) {
  Permutations perms(6);

  while (perms.has_next()) {
    vector<int> perm = perms.next();
    for (int i : perm) {
      cout << i;
    }
    cout << endl;
  }
  return 0;
}
