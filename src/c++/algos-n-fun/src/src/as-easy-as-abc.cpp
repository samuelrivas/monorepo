#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_set>
#include <stack>
#include <sstream>
#include <cassert>

#include "lib/digraph.hpp"

using std::vector;
using std::min;
using std::cout;
using std::endl;
using std::unordered_set;
using std::stack;
using std::ostringstream;

int node_number(char x) {
  return x - 'a' + 1;
}

char node_to_char(int x) {
  return x - 1 + 'a';
}

int get_radix(vector<string> dict) {
  unordered_set<char> chars;
  for (string s : dict) {
    for (char c : s) {
      chars.insert(c);
    }
  }
  return chars.size();
}

Digraph get_order(const vector<string>& samples, int radix) {
  Digraph graph(radix + 1);
  for (int i = 1; i <= radix; i++) {
    graph.connect(0, i);
  }
  for (size_t i = 1; i < samples.size(); i++) {
    string low = samples[i - 1];
    string high = samples[i];
    size_t min_length = min(low.size(), high.size());

    size_t j;
    for (j = 0; j < min_length && low[j] == high[j]; j++) { }

    if (j < min_length) {
      // We can add duplicate edges here
      graph.connect(node_number(low[j]), node_number(high[j]));
    }
  }
  return graph;
}

class AbcCallbacks : public DfsCallbacks {
  public:
  stack<int> topological;
  bool cycle = false;

  virtual void on_edge(int from, int to, const vector<int>& parent, const vector<State>& state) override {
    if (state[to] == State::Processing) {
      cycle = true;
    }
    (void) from;
    (void) parent;
  }

  virtual void on_exit(int vertex, const vector<int>& parent, const vector<State>& state) override {
    topological.push(vertex);

    (void) parent;
    (void) state;
  }
};

int main(void) {
  vector<vector<string>> tests {
    { "aab", "aac", "ab", "cabc" },
    { "cab", "cda", "ccc", "badca" },
    { "abc", "bca", "cab", "aca" },
    { "dea", "cfb" },
    { "ebbce", "dbe", "adcd", "bc", "cd" }
  };

  for (vector<string> test : tests) {
    for (string s : test) {
      cout << s << endl;
    }
    Digraph graph = get_order(test, get_radix(test));
    AbcCallbacks callbacks;
    cout << graph.to_s() << endl;
    cout << endl;

    Dfs dfs(graph, &callbacks);
    dfs.dfs(0);

    if (callbacks.cycle) {
      cout << "IMPOSSIBLE" << endl;
      continue;
    }

    ostringstream order;
    assert(callbacks.topological.top() == 0);
    callbacks.topological.pop();

    int previous = 0;
    while(! callbacks.topological.empty()) {
      int vertex = callbacks.topological.top();
      callbacks.topological.pop();

      if (!graph.connected(previous, vertex)) {
        // This is not a hamiltonian path, thus we don't have a total order
        cout << "AMBIGUOUS" << endl;
        break; // TODO FIX
      }
      order << node_to_char(vertex);
      previous = vertex;
    }
    cout << endl;
    cout << "order: " << order.str() << endl << endl;
  }
  return 0;
}
