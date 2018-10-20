#include <iostream>
#include <vector>
#include <iomanip>
#include <cassert>

using std::vector;
using std::cout;
using std::endl;
using std::setw;

vector<int> next_ls(const vector<int>& current) {
  assert(current.size() > 0) ;

  vector<int> result;
  int stride_length = 1;
  int stride_element = current[0];

  for (size_t i = 1; i < current.size(); i++) {
    if (current[i] == stride_element) {
      stride_length++;
    } else {
      result.push_back(stride_length);
      result.push_back(stride_element);
      stride_length = 1;
      stride_element = current[i];
    }
  }
  result.push_back(stride_length);
  result.push_back(stride_element);

  return result;
}

int main(void) {
  vector<int> current { 1 };
  for (int i = 0; i < 10; i++) {
    for (int element : current) {
      cout << setw(3) << element;
    }
    cout << endl;
    current = next_ls(current);
  }
  return 0;
}
