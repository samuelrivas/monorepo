/* Copyright 2018 <samuelrivas@gmail.com
 */
#include <vector>
#include <iostream>
#include <string>
#include <unordered_set>
#include <utility>

using std::vector;
using std::cout;
using std::endl;
using std::string;
using std::unordered_multiset;
using std::pair;

bool anagrams(const string a, const string b) {
  unordered_multiset<char> chars;

  for (const char c : a) {
    chars.insert(c);
    cout << "inserted " << c << endl;
  }

  for (const char c : b) {
    if (chars.count(c) == 0) {
      cout << "couldn't find " << c <<endl;
      return false;
    } else {
      chars.erase(chars.find(c));
      cout << "erased " << c << endl;
    }
  }

  return chars.empty();
}

int main(void) {
  vector<pair<string, string>> tests {
    {"foo", "fooo"},
    {"fooo", "foo"},
    {"", "foo"},
    {"foo", ""},
    {"", ""},
    {"foo", "ofo"},
    {"listen", "silent"},
    {"samuel", "rivas"}
  };

  for (auto test : tests) {
    cout << "("  << test.first << "," << test.second
         << ") -> " << anagrams(test.first, test.second) << endl;
  }
  return 0;
}
