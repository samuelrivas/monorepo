/* Copyright (C) 2018 by samuelrivas@gmail.com
 *
 * Solution for https://open.kattis.com/problems/easyascab
 */
#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_set>
#include <stack>
#include <sstream>
#include <cassert>
#include <string>

#include <digraph.hpp>

using std::vector;
using std::min;
using std::cout;
using std::cin;
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
      /* We can add duplicate edges here, but should be fine unless we are
         dealing with a large amount of samples.

         If that were a problem, we should either keep a hash set of added edges
         here, or reimplement the graph to store sets instead of lists of
         connections
      */
      graph.connect(node_number(low[j]), node_number(high[j]));
    }
  }
  return graph;
}

class AbcCallbacks : public DfsCallbacks {
 public:
  stack<int> topological;
  bool cycle = false;

  void on_edge(int from, int to,
               const vector<int>& parent,
               const vector<State>& state) override {
    if (state[to] == State::Processing) {
      cycle = true;
    }
    (void) from;
    (void) parent;
  }

  void on_exit(int vertex,
               const vector<int>& parent,
               const vector<State>& state) override {
    topological.push(vertex);

    (void) parent;
    (void) state;
  }
};

void load_params(int* radix, vector<string>* samples) {
  char highchar;
  cin >> highchar;
  *radix = highchar - 'a' + 1;

  int n;
  cin >> n;
  for (int i = 0; i < n; i++) {
    string sample;
    cin >> sample;
    samples -> push_back(sample);
  }
}

int main(void) {
  int radix;
  vector<string> samples;

  load_params(&radix, &samples);

  Digraph graph = get_order(samples, radix);
  AbcCallbacks callbacks;

  Dfs dfs(graph, &callbacks);
  dfs.dfs(0);

  if (callbacks.cycle) {
    cout << "IMPOSSIBLE" << endl;
    return 0;
  }

  ostringstream order;
  assert(callbacks.topological.top() == 0);
  callbacks.topological.pop();

  int previous = 0;
  while (!callbacks.topological.empty()) {
    int vertex = callbacks.topological.top();
    callbacks.topological.pop();

    if (!graph.connected(previous, vertex)) {
      // This is not a hamiltonian path, thus we don't have a total order
      cout << "AMBIGUOUS" << endl;
      return 0;
    }

    order << node_to_char(vertex);
    previous = vertex;
  }
  cout << order.str() << endl;

  return 0;
}
