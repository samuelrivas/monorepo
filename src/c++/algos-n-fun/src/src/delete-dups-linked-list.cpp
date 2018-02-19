/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Delete duplicates in a linked list
 */
#include <iostream>
#include <forward_list>
#include <unordered_set>
#include <vector>
#include <iomanip>

using std::unordered_set;
using std::forward_list;
using std::cout;
using std::endl;
using std::vector;
using std::setw;

template <typename T>
void dedup(forward_list<T>* l) {
  unordered_set<T> seen;
  typename std::forward_list<T>::const_iterator before_it = l -> cbefore_begin();
  typename std::forward_list<T>::const_iterator it = l -> cbegin();

  while(it  != l -> end()) {
    T element = *(it);
    if (seen.find(element) == seen.end()) {
      seen.insert(element);
      it++;
      before_it++;
    } else {
      it = l -> erase_after(before_it);
    }
  }
}

int main() {
  vector<forward_list<int>> tests {
    { },
    {1, 1, 1},
    {1, 2, 1, 3 },
    {2, 1, 2, 2, 1, 1, 3, 3},
    { 1, 2, 3 }
  };

  for (forward_list<int> test : tests) {
    for (int i : test) {
      cout << setw(2) << i;
    }
    cout << " ->";
    dedup(&test);
    for (int i : test) {
      cout << setw(2) << i;
    }
    cout << endl;
  }
}
