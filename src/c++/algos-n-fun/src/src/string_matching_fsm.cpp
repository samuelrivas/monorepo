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
 * This is linear on the length of query (assuming that the radix is irrelevant,
 * but could be even "more linear" at the cost of memory if we used an array
 * instead of a map to store the possible characters (which would let us use an
 * array for "seen" too, but make the radix a lot more relevant)
 */
vector<unordered_map<char, int>> query_fsm(string query) {
  vector<unordered_map<char, int>> fsm(query.length());

  /* Mismatched_satate is the state of a machine that has matched all but the
   * first character of the pattern
   */
  unordered_set<char> seen { query[0] };

  int mismatched_state = 0;
  fsm[0][query[0]] = 1;

  for (size_t i = 1; i < query.length(); i++) {
    // Mismatcthing transitions
    for (char c : seen) {
      fsm[i][c] = fsm[mismatched_state][c];
    }

    // Matching transition
    fsm[i][query[i]] = i + 1;
    mismatched_state = fsm[mismatched_state][query[i]];
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
    "ababac",
    "abcabc",
    "abba",
    "aaaabaaaab",
    "the quick brown fox"
  };

  for (auto test : tests) {
    cout << test << ":" << endl;
    print_fsm(query_fsm(test));
  }
  return 0;
}
