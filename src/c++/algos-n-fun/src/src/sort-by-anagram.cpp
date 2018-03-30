// Copyright (C) 2018 by samuelrivas@gmail.com
#include <string>
#include <algorithm>
#include <utility>
#include <vector>
#include <cassert>
#include <iostream>

using std::string;
using std::sort;
using std::move;
using std::pair;
using std::vector;
using std::cout;
using std::endl;

string sort_string(string x) {
  string result(move(x));
  sort(result.begin(), result.end());
  return result;
}

struct comp_op {
  bool operator()(const pair<string, string>& x,
                  const pair<string, string>& y) {
    return x.first < y.first;
  }
} comp;

void sort_by_anagram(vector<string>* strings) {
  vector<pair<string, string>> with_signatures(strings -> size());

  for (size_t i = 0; i < strings -> size(); i++) {
    string s((*strings)[i]);
    with_signatures[i] = pair<string, string>(sort_string(s), s);
  }
  sort(with_signatures.begin(), with_signatures.end(), comp);

  assert(with_signatures.size() == strings -> size());

  for (size_t i = 0; i < strings -> size(); i++) {
    (*strings)[i] = with_signatures[i].second;
  }
}

int main(void) {
  vector<vector<string>> tests {
    { "anagram", "foo", "nagaram", "bar", "ofo" },
    { }
  };

  for (vector<string> test : tests) {
    sort_by_anagram(&test);
    for (string x : test) {
      cout << x << endl;
    }
    cout << endl;
  }
  return 0;
}
