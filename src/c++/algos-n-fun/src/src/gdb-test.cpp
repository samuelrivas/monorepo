#include <iostream>
#include <vector>

using std::vector;

int main(void) {

  vector<int> tests = { 1, 2, 3, 4, 5, 6 };
  for (auto i : tests) {
    std::cout << i + 5 << std::endl;
  }

  std::cout << "done" << std::endl;

  return 0;
}