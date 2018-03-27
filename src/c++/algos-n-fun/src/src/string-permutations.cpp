// Copyright (C) 2018 by samuelrivas@gmail.com

#include<string>
#include<vector>
#include<unordered_set>
#include<utility>
#include<iostream>

using std::string;
using std::vector;
using std::unordered_set;
using std::move;
using std::cout;
using std::endl;

void permutations(const string& x, string current_prefix, vector<bool> taken,
                  unordered_set<string>* results) {
  bool added = false;

  for (size_t i = 0; i < x.size(); i++) {
    if (!taken[i]) {
      string new_prefix(current_prefix);
      new_prefix.push_back(x[i]);

      vector<bool> new_taken(taken);
      new_taken[i] = true;

      added = true;

      permutations(x, move(new_prefix), move(new_taken), results);
    }
  }
  if (!added) {
    results -> insert(current_prefix);
  }
}

unordered_set<string> permutations(const string& x) {
  unordered_set<string> results;
  vector<bool> taken(x.size(), false);

  permutations(x, "", move(taken), &results);
  return results;
}

int main(void) {
  vector<string> tests {
    "",
    "foo",
    "samu"
  };

  for (string test : tests) {
    cout << "'" << test << "':" << endl;

    for (string perm : permutations(test)) {
      cout << perm << endl;
    }
  }
  return 0;
}
