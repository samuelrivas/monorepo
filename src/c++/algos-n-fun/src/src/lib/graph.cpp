// Copyright (C) 2018 by samuelrivas@gmail.com
#include <forward_list>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include <cassert>

#include "graph.hpp"
#include "digraph.hpp"

using std::forward_list;
using std::ostringstream;
using std::string;

Graph::Graph(int n_vertices) :
  digraph(n_vertices) { }

void Graph::connect(int a, int b) {
    digraph.connect(a, b);
    digraph.connect(b, a);
}

int Graph::n_vertices() const {
  return digraph.n_vertices();
}

forward_list<int> Graph::connected(int vertex) const {
  return digraph.connected(vertex);
}

bool Graph::connected(int x, int y) {
  return digraph.connected(x, y);
}

string Graph::to_s() const {
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

// TODO Make Dfs and Gfs work for both graphs and digraphs
Bfs::Bfs(const Graph& _graph, BfsCallbacks* _callbacks) :
  visited(_graph.n_vertices(), false),
  processed(_graph.n_vertices(), false),
  state(_graph.n_vertices(), State::Unprocessed),
  parent(_graph.n_vertices(), -1),
  callbacks { _callbacks },
  graph { _graph } { }

void Bfs::bfs(int vertex) {
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
      if (parent[v] != child && state[child] != State::Processed) {
        callbacks -> on_edge(v, child, parent, state);
      }

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
