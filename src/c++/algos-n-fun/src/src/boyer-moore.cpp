/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Fast stream matching (not with streaming
 */

#include <iostream>
#include <unordered_map>
#include <vector>
#include <string>
#include <iomanip>

using std::unordered_map;
using std::vector;
using std::string;
using std::cout;
using std::endl;
using std::setw;

unordered_map<char, vector<int>> prepare_table(string pattern) {
  unordered_map<char, vector<int>> table;

  for (char c : pattern) {
    if (table.find(c) == table.end()) {
      table[c] = vector<int>(pattern.size());
      int count = pattern.size();
      for (size_t i = 0; i < pattern.size(); i++) {
        if (pattern[i] == c) {
          count = 0;
        }
        table[c][i] = count;
        if (count != -1) {
          count++;
        }
      }
    }
  }
  return table;
}

void print_table(unordered_map<char, vector<int>> table) {
  for (auto kv : table) {
    cout << kv.first << " ";

    for (int count : kv.second) {
      cout << setw(3) << count;
    }
    cout << endl;
  }
}

int main(void) {
  vector<string> tests = {
    "aaaab",
    "ababc",
    "ababac",
    "abcabc",
    "abba",
    "aaaabaaaab",
    "the quick brown fox"
  };

  for (auto test : tests) {
    cout << test << ":" << endl;
    print_table(prepare_table(test));
  }
  return 0;
}
