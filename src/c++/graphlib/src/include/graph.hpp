// Copyright (C) 2018 by samuelrivas@gmail.com
#ifndef _GRAPH_H_
#define _GRAPH_H_

#include <forward_list>
#include <string>
#include <vector>
#include <queue>

#include "digraph.hpp"

using std::forward_list;
using std::string;
using std::queue;

class Graph {
  Digraph digraph;

 public:
  explicit Graph(int n_vertices);

  void connect(int a, int b);
  int n_vertices() const;
  forward_list<int> connected(int vertex) const;
  bool connected(int x, int y);
  string to_s() const;
};

typedef DfsCallbacks BfsCallbacks;

// TODO Make Dfs and Bfs work for both graphs and digraphs
class Bfs {
  vector<bool> visited;
  vector<bool> processed;
  vector<State> state;
  vector<int> parent;
  BfsCallbacks *callbacks;
  const Graph& graph;
  queue<int> to_process;

 public:
  Bfs(const Graph& _graph, BfsCallbacks* _callbacks);

  void bfs(int vertex);
};

#endif
