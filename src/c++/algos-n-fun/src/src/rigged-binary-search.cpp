#include <vector>
#include <iostream>
#include <iomanip>
#include <cassert>

using std::vector;
using std::cout;
using std::setw;
using std::endl;

bool is_it_there(const vector<int>& buff, int x, size_t low_index, size_t size) {
  assert(buff.size() > low_index + size - 1);

  if (size == 0) {
    return false;
  }

  if (buff[low_index] > buff[low_index + size - 1]) {
    // We have a rotation point inside
    return x >= buff[low_index] || x <= buff[low_index + size - 1];
  } else {
    return x >= buff[low_index] && x <= buff[low_index + size - 1];
  }
}

int find(const vector<int>& buff, int x, size_t low_index, size_t size) {
  assert(size > 0);

  if (size == 1) {
    if (buff[low_index] == x) {
      return low_index;
    } else {
      return - 1;
    }
  }

  size_t new_size = size / 2;

  if (is_it_there(buff, x, low_index, new_size)) {
    return find(buff, x, low_index, new_size);
  } else if (is_it_there(buff, x, low_index + new_size, size - new_size)) {
    return find(buff, x, low_index + new_size, size - new_size);
  } else {
    return -1;
  }
}

int find(const vector<int>& buff, int x) {
  return find(buff, x, 0, buff.size());
}

int main(void) {
  vector<int> buff { 15, 16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14 };

  vector<int> tests { 20, 25, 1, 3, 14, 15, 16, 18, 6, 0 };

  for (int test : tests) {
    int pos = find(buff, test);
    cout << setw(3) << test << ":" << pos;
    if (pos < 0) {
      cout << " (XX)" << endl;
    } else {
      cout << "(" << buff[pos] << ")" << endl;
    }
  }
  return 0;
}
