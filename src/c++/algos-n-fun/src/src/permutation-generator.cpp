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

int factorial(int n) {
  int result = 1;
  for (int i = 2; i <=n; i++) {
    result *= i;
  }
  return result;
}

class Permutations {
  vector<int> current;
  int left;
  int elements;
  bool first;

public:
  explicit Permutations(int _elements) :
    current(_elements),
    elements { _elements },
    first { true } {

    assert(_elements > 0);

    for (int i = 0; i < elements; i++) {
      current[i] = elements - i;
      left = factorial(elements);
    }
  }
  vector<int> next() {
    assert(left > 0);
    left--;

    // UGh
    if (first) {
      first = false;
      return current;
    }

    set<int> seen;
    seen.insert(current[0]);

    int pos_to_change = 1;
    int max_seen = current[0];
    while (max_seen <= current[pos_to_change]) {
      assert(pos_to_change < elements);

      seen.insert(current[pos_to_change]);
      max_seen = max(max_seen, current[pos_to_change]);
      pos_to_change++;
    }
    cerr << "found at " << pos_to_change << ":" << current[pos_to_change]
         << endl;

    int to_insert = *(seen.lower_bound(current[pos_to_change]));
    cerr << "going to insert " << to_insert
         << endl;

    seen.insert(current[pos_to_change]);
    seen.erase(to_insert);
    current[pos_to_change] = to_insert;

    assert(seen.size() == static_cast<size_t>(pos_to_change));
    int pos_to_insert = pos_to_change - 1;
    for (int i : seen) {
      current[pos_to_insert] = i;
      pos_to_insert--;
    }
    return current;
  }

  bool has_next() {
    return left > 0;
  }
};

int main(void) {
  Permutations perms(4);

  while (perms.has_next()) {
    vector<int> perm = perms.next();
    for (int i : perm) {
      cout << i;
    }
    cout << endl;
  }
  return 0;
}
