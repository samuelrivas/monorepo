// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <vector>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include <utility>

#include "lib/sort.hpp"

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::swap;
using std::setw;
using std::pair;

int main(void) {
  vector<pair<vector<int>, int>> tests = {
    {{ 1, 1, 1, 1 }, 1},
    {{ 1, 1, 1, 1 }, 3},
    {{ 1, 1, 1, 1 }, 4},
    {{ 2, 1, 2, 3, 3, 1, 2 }, 2},
    {{ 2, 1, 2, 3, 3, 1, 2 }, 4},
    {{ 2, 1, 2, 3, 3, 1, 2 }, 6},
    {{ 3, 2, 3, 2 }, 1},
    {{ 3, 2, 3, 2 }, 2},
    {{ 3, 2, 3, 2 }, 3},
    {{ 9, 7, 10, 3, 6, 4, 0, 1, 8, 2, 5 }, 1},
    {{ 9, 7, 10, 3, 6, 4, 0, 1, 8, 2, 5 }, 10},
    {{ 9, 7, 10, 3, 6, 4, 0, 1, 8, 2, 5 }, 5},
    {{ 1, 1, 4, 0, 3, 4, 0, 2, 5, 5, 5, 1, 3, 5, 1, 5, 2, 0, 1, 0 }, 3},
    {{ 1, 1, 4, 0, 3, 4, 0, 2, 5, 5, 5, 1, 3, 5, 1, 5, 2, 0, 1, 0 }, 10}
  };

  for (auto test : tests) {
    cout << "Find " << test.second << ":";
    for (int i : test.first) {
      cout << setw(3) << i;
    }
    cout << endl;

    int result = selection<int>(&(test.first), test.second);
    cout << "Result: " << result << endl;
    cout << "Final Vector:";
    for (int i : test.first) {
      cout << setw(3) << i;
    }
    cout << ")" << endl << endl;
  }
  return 0;
}
