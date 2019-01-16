// Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>
#include <vector>

#include <graph.hpp>

using std::endl;
using std::cout;

class TestCallbacks : public BfsCallbacks {
  void on_edge(int vertex, int to, const vector<int>& parent,
               const vector<State>& state) override {
    (void) parent;

    if (state[to] == State::Processing) {
      cout << vertex << " -- " << to << " creates a cycle" << endl;
    } else {
      cout << vertex << " -- " << to << endl;
    }
  }

  void on_entry(int vertex, const vector<int>& parent,
                const vector<State>& state) override {
    (void) state;
    (void) parent;

    cout << "Starting: " << vertex << endl;
  }

  void on_exit(int vertex, const vector<int>& parent,
               const vector<State>& state) override {
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
