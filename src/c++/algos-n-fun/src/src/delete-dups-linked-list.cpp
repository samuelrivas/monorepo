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
  typename std::forward_list<T>::const_iterator before_it = l ->cbefore_begin();
  typename std::forward_list<T>::const_iterator it = l -> cbegin();

  while (it  != l -> end()) {
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

template <typename T>
void dedup_mem_stingy(forward_list<T>* l) {
  auto before_it = l -> cbefore_begin();
  auto it = l -> cbegin();

  before_it++;
  it++;

  while (it != l -> end()) {
    auto find_dup = l -> cbegin();
    while (find_dup != before_it && *find_dup != *it) {
      find_dup++;
    }

    if (*find_dup == *it) {
      it = l -> delete_after(before_it);
    } else {
      it++;
      before_it++;
    }
  }
}

int main() {
  vector<forward_list<int>> tests_1 {
    { },
    {1, 1, 1},
    {1, 2, 1, 3 },
    {2, 1, 2, 2, 1, 1, 3, 3},
    { 1, 2, 3 }
  };

  vector<forward_list<int>> tests_2 {
    { },
    {1, 1, 1},
    {1, 2, 1, 3 },
    {2, 1, 2, 2, 1, 1, 3, 3},
    { 1, 2, 3 }
  };

  cout << "Test 1:" << endl;
  for (forward_list<int> test : tests_1) {
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

  cout << "Test 2:" << endl;
  for (forward_list<int> test : tests_2) {
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
