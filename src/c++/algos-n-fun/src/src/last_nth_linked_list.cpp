/* Copyright 2018 <samuelrivas@gmail.com
 *
 * find last nth element in a linked list
 */
#include <forward_list>
#include <cassert>
#include <iostream>
#include <vector>
#include <utility>

using std::forward_list;
using std::pair;
using std::vector;
using std::cout;
using std::endl;

template <typename T>
bool nth_last(const forward_list<T> &l, int n, T* out) {
  assert(n > 0);

  auto head = l.cbegin();
  for (int i = 1; i < n && head != l.cend(); i++, head++) { }

  if (head == l.cend()) {
    return false;
  }

  head++;

  auto nth_last = l.cbegin();
  for (; head != l.cend(); head++, nth_last++) { }
  *out = *nth_last;

  return true;
}

int main(void) {
  vector<pair<forward_list<int>, int>> tests {
    { { }, 1 },
    { { 1, 2, 3 }, 1 },
    { { 1, 2, 3 }, 2 },
    { { 1, 2, 3 }, 3 },
    { { 1, 2, 3 }, 4 }
  };

  for (auto test : tests) {
    int nth;
    bool success = nth_last(test.first, test.second, &nth);
    cout << "Finding " << test.second << " last in: ";
    for (int i : test.first) {
      cout << i << " ";
    }
    cout << " -> ";

    if (success) {
      cout << nth;
    } else {
      cout << "none!";
    }
    cout << endl;
  }
}
