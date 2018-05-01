#include <forward_list>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include <queue>
#include <cassert>

#include "lib/digraph.hpp"

using std::forward_list;
using std::ostringstream;
using std::string;
using std::endl;
using std::cout;
using std::queue;

class Graph {
  Digraph digraph;

 public:
  explicit Graph(int n_vertices) :
    digraph(n_vertices) { }

  void connect(int a, int b) {
    digraph.connect(a, b);
    digraph.connect(b, a);
  }

  int n_vertices() const {
    return digraph.n_vertices();
  }

  forward_list<int> connected(int vertex) const {
    return digraph.connected(vertex);
  }

  bool connected(int x, int y) {
    return digraph.connected(x, y);
  }

  string to_s() const {
    vector<bool> shown(n_vertices(), false);
    ostringstream out;

    for (int vertex = 0; vertex < n_vertices(); vertex++) {
      if (!shown[vertex]) {
        shown[vertex] = true;
        for (int x : connected(vertex)) {
          if (!shown[x]) {
            out << vertex << " -- " << x << endl;
          }
        }
      }
    }
    return out.str();
  }
};

typedef DfsCallbacks BfsCallbacks;

class Bfs {
  vector<bool> visited;
  vector<bool> processed;
  vector<State> state;
  vector<int> parent;
  BfsCallbacks *callbacks;
  const Graph& graph;
  queue<int> to_process;

 public:
  Bfs(const Graph& _graph, BfsCallbacks* _callbacks) :
    visited(_graph.n_vertices(), false),
    processed(_graph.n_vertices(), false),
    state(_graph.n_vertices(), State::Unprocessed),
    parent(_graph.n_vertices(), -1),
    callbacks { _callbacks },
    graph { _graph } { }

  void bfs(int vertex) {
    if (state[vertex] == State::Processed) {
      return;
    }

    assert(state[vertex] == State::Unprocessed);

    to_process.push(vertex);

    while (!to_process.empty()) {
      int v = to_process.front();
      to_process.pop();

      callbacks -> on_entry(v, parent, state);
      state[v] = State::Processing;

      for (int child : graph.connected(v)) {
        callbacks -> on_edge(v, child, parent, state);

        if (state[child] == State::Unprocessed) {
          to_process.push(child);
          state[child] = State::Processing;
          parent[child] = v;
        }
      }
      callbacks -> on_exit(v, parent, state);
      state[v] = State::Processed;
    }
  }
};

class TestCallbacks : public BfsCallbacks {
  void on_edge(int vertex, int to, const vector<int>& parent, const vector<State>& state) {
    if (parent[vertex] == to) {
      // Traversing the link to parent (thus second time)
      return;
    }

    if (state[to] == State::Processed) {
      // Traversing a cycle link for the second time
      return;
    }

    if (state[to] == State::Processing) {
      cout << vertex << " -- " << to << " creates a cycle (b)" << endl;
    }
  }

  void on_entry(int vertex, const vector<int>& parent, const vector<State>& state) override {
    (void) state;
    (void) parent;

    cout << "Starting: " << vertex << endl;
  }

  void on_exit(int vertex, const vector<int>& parent, const vector<State>& state) override {
    (void) state;

    cout << "Finished: " << vertex;

    for (int v = parent[vertex]; v != -1; v = parent[v]) {
      cout << " -- " << v;
    }
    cout << endl;
  }
} callbacks;

int main(void) {
  Graph graph(8);

  graph.connect(7, 6);
  graph.connect(0, 1);
  graph.connect(0, 2);
  graph.connect(5, 1);
  graph.connect(3, 1);
  graph.connect(3, 2);
  graph.connect(3, 4);
  graph.connect(6, 0);
  graph.connect(3, 7);

  cout << graph.to_s();

  cout << "BFS:" << endl;
  Bfs bfs(graph, &callbacks);
  bfs.bfs(2);

  return 0;
}
