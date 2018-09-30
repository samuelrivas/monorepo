// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <vector>
#include <unordered_set>
#include <utility>
#include <optional>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::unordered_set;
using std::pair;

pair<int, int> find_matching_pair(int target, const vector<int>& numbers) {
  unordered_set<int> seen;

  for (int number : numbers) {
    if (seen.find(target - number) != seen.end()) {
      return { target - number, number };
    }
    seen.insert(number);
  }
  return {0, 0};
}

int main(void) {
  vector<pair<vector<int>, int>> tests {
    {{1, 3, 5, 2, 4}, 7},
    {{1, 3, 5, 2, 4}, 0},
    {{1, -3, 4, 3, 0}, 0}};

  for (auto test : tests) {
    auto result = find_matching_pair(test.second, test.first);
    cout << result.first << ", " << result.second << endl;
  }
}
