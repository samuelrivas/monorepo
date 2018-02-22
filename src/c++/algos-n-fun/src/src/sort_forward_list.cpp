/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Sort a linked list (ugh!)
 */
#include <iostream>
#include <forward_list>
#include <vector>
#include <iomanip>

using std::forward_list;
using std::cout;
using std::endl;
using std::vector;
using std::setw;

// Horribly quadratic in most cases, obviously. Unless the original list is
// partially reverse ordered
template <typename T>
void sort_flist(forward_list<T>* l) {
  auto before_head = l -> before_begin();
  auto head = l -> begin();

  while (head != l -> end()) {
    // find where we need to insert head to keep everything to the left of the
    // current head position sorted
    auto element = l -> begin();
    while (*element <= *head && element != head) {
      element++;
    }

    if (*element > *head) {
      // Insert a node after us, put this element there and take head to here
      l -> insert_after(element, *element);
      *element = *head;

      // Remove the old head, we don't need to advance here
      head = l -> erase_after(before_head);
    } else {
      before_head++;
      head++;
    }
  }
}

int main(void) {
  vector<forward_list<int>> tests {
    { },
    { 1, 2, 3 },
    { 1, 1, 1 },
    { 3, 2, 1 },
    { 1 },
    { 8, 5, 1, 1, 5, 9, 0 }
  };

  for (auto test : tests) {
    for (int i : test) {
      cout << setw(2) << i;
    }

    cout << " ->";

    sort_flist(&test);

    for (int i : test) {
      cout << setw(2) << i;
    }

    cout << endl;
  }
  return 0;
}
