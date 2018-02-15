/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Create the FSM for streaming string matching
 */

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <iomanip>

using std::string;
using std::vector;
using std::unordered_map;
using std::unordered_set;
using std::set;
using std::cout;
using std::endl;
using std::setw;

/* fsm[i][c] returns the amount of matched characters after reading
 * c, given that the previous amount was i
 *
 * This is quadratic on the length o query
 */
vector<unordered_map<char, int>> query_fsm(string query) {
  vector<unordered_map<char, int>> fsm(query.length());

  // first pass setting the forward links 

  for (size_t i = 0; i < query.length(); i++) {
    fsm[i][query[i]] = i + 1;
  }

  // second pass setting the backward links
  unordered_set<char> seen;
  for (size_t i = 0; i < query.length(); i++) {
    for (char c : seen) {
      if (fsm[i][c] == 0) {
        int matched = 0;
        for (size_t j = 1; j < i; j++) {
          matched = fsm[matched][query[j]];
        }
        matched = fsm[matched][c];
        if (matched > 0) {
          fsm[i][c] = matched;
        }
      }
    }
    seen.insert(query[i]);
  }
  return fsm;
}

void print_fsm(vector<unordered_map<char, int>> fsm) {
  // Collect all possible characters (alternatively pass the pattern)
  set<char> chars;
  for (auto transition : fsm) {
    for (auto val : transition) {
      chars.insert(val.first);
    }
  }

  // print header statuses
  cout << "   ";
  for (size_t i = 0; i < fsm.size(); i++) {
    cout << setw(3) << i;
  }
  cout << endl;

  // for each char, print char and transitions
  for (char c : chars) {
    cout << c << "  ";
    for (size_t i = 0; i < fsm.size(); i++) {
      cout << setw(3) << fsm[i][c];
    }
    cout << endl;
  }
}

int main(void) {
  vector<string> tests = {
    "aaaab",
    "ababc",
    "abcabc",
    "abba",
    "aaaabaaaab",
    "the quick brown fox jumps over the lazy dog"
  };

  for(auto test : tests) {
    cout << test << ":" << endl;
    print_fsm(query_fsm(test));
  }
  return 0;
}