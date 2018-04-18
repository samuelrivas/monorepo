/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Homegrown directed graphs (as adjacency lists)
 */
#include <vector>
#include <forward_list>
#include <cassert>

#include "digraph.hpp"

using std::vector;
using std::forward_list;

Digraph::Digraph(int n_vertices) : vertices(n_vertices) {
}

void Digraph::connect(int from, int to) {
  vertices[from].push_front(to);
}

int Digraph::n_vertices() const {
  return vertices.size();
}

forward_list<int> Digraph::connected(int vertex) const {
  return vertices[vertex];
}

string Digraph::to_s() const {
  ostringstream out;

  for (size_t from = 0; from < vertices.size(); from++ ) {
    for (int to : connected(from)) {
      out << from << " -> " << to << endl;
    }
  }
  return out.str();
}

void DfsCallbacks::on_processed(int vertex, const vector<int>& parent,
                                const vector<bool>& processed) const {
  (void) vertex;
  (void) parent;
  (void) processed;
}

void DfsCallbacks::on_seen(int vertex, const vector<int>& parent,
                           const vector<bool>& processed) const {
  (void) vertex;
  (void) parent;
  (void) processed;
}

Dfs::Dfs(const Digraph& _digraph,
         const DfsCallbacks* _callbacks) :
  visited(_digraph.n_vertices(), false),
  processed(_digraph.n_vertices(), false),
  parent(_digraph.n_vertices(), -1),
  callbacks { _callbacks },
  digraph { _digraph } {
}

#include <iostream>

void Dfs::dfs(int vertex) {
  assert(!visited[vertex]);

  visited[vertex] = true;
  // std::cout << "visited " << vertex << std::endl;

  for (int v : digraph.connected(vertex)) {
    callbacks -> on_seen(v, parent, processed);
    if (visited[v]) {
      // std::cout << "found already visited: " << v << std::endl;
      if (!processed[v]) {
        std::cout << "and this is a cycle!" << std::endl;
        std::cout << v << " <- ";
        for (int offender = vertex; offender != v; offender = parent[offender]) {
          std::cout << offender << " <- ";
        }
        std::cout << v << std::endl;
      }
      continue;
    }
    parent[v] = vertex;
    dfs(v);
  }
  std::cout << "covered: " << vertex << std::endl;
  processed[vertex] = true;
  callbacks -> on_processed(vertex, parent, processed);
}

const vector<int> Dfs::parents() const {
  return parent;
}
