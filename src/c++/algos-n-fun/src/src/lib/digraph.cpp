/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Homegrown directed graphs (as adjacency lists)
 */
#include <vector>
#include <forward_list>

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

Dfs::Dfs(const Digraph& _digraph) :
  visited(_digraph.n_vertices(), false),
  processed(_digraph.n_vertices(), false),
  parent(_digraph.n_vertices(), -1),
  digraph { _digraph } {
}

#include <iostream>

void Dfs::dfs(int vertex) {
  if (visited[vertex]) {
    std::cout << "This should not happen: " << vertex << std::endl;
    return;
  }
  visited[vertex] = true;
  // std::cout << "visited " << vertex << std::endl;

  for (int v : digraph.connected(vertex)) {
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
  covered++;
}

const vector<int> Dfs::parents() const {
  return parent;
}

int Dfs::covered_vertices() const {
  return covered;
}
