// Copyright (C) 2018 by samuelrivas@gmail.com
#include <vector>
#include <cassert>
#include <iostream>
#include <iomanip>

using std::vector;
using std::cout;
using std::endl;
using std::setw;

// Merge orig and dest, in place in dest. Assumed increasing order
void merge(const vector<int>& orig, vector<int>* dest, size_t orig_pos,
           size_t dest_pos, size_t copy_pos) {
  size_t orig_size = orig.size();
  size_t dest_size = dest -> size();

  while (orig_pos < orig_size && dest_pos < dest_size) {
    if (orig[orig_pos] > (*dest)[dest_pos]) {
      (*dest)[copy_pos--] = orig[orig_pos--];
    } else {
      (*dest)[copy_pos--] = (*dest)[dest_pos--];
    }
  }

  while (orig_pos < orig_size) {
    (*dest)[copy_pos--] = orig[orig_pos--];
  }
}

void merge(const vector<int>& orig, vector<int>* dest, size_t dest_elements) {
  assert(dest_elements + orig.size() <= dest -> size());

  merge(orig, dest, orig.size() - 1, dest_elements - 1,
        dest_elements + orig.size() - 1);
}

int main(void) {
  vector<int> orig { 3, 6 };
  vector<int> dest { 1, 5, 7, 0, 0, 0, 0 };

  merge(orig, &dest, 3);

  for (int x : dest) {
     cout << setw(2) << x;
  }
  cout << endl;

  return 0;
}
