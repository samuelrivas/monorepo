/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Homegrown directed graphs (as adjacency lists)
 */
#include <vector>
#include <forward_list>

#include "digraph.hpp"

using std::vector;
using std::forward_list;

Digraph::Digraph(size_t n_vertices) : vertices(n_vertices) {
}

void Digraph::connect(size_t from, size_t to) {
  vertices[from].push_front(to);
}

int Digraph::n_vertices() const {
  return vertices.size();
}

forward_list<size_t> Digraph::connected(size_t vertex) const {
  return vertices[vertex];
}

string Digraph::to_s() const {
  ostringstream out;

  for (size_t from = 0; from < vertices.size(); from++ ) {
    for (size_t to : connected(from)) {
      out << from << " -> " << to << endl;
    }
  }
  return out.str();
}

Dfs::Dfs(const Digraph& _digraph) :
  visited(_digraph.n_vertices(), false),
  processed(_digraph.n_vertices(), false),
  parent(_digraph.n_vertices()),
  digraph { _digraph } {
}

#include <iostream>

void Dfs::dfs(size_t vertex) {
  if (visited[vertex]) {
    std::cout << "This should not happen: " << vertex << std::endl;
    return;
  }
  visited[vertex] = true;
  // std::cout << "visited " << vertex << std::endl;

  for (size_t v : digraph.connected(vertex)) {
    if (visited[v]) {
      // std::cout << "found already visited: " << v << std::endl;
      if (!processed[v]) {
        std::cout << "and this is a cycle!" << std::endl;
        std::cout << v << " <- ";
        for (size_t offender = vertex; offender != v; offender = parent[offender]) {
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

const vector<size_t> Dfs::parents() const {
  return parent;
}

int Dfs::covered_vertices() const {
  return covered;
}
