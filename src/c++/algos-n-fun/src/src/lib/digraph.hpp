/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Homegrown directed graphs (as adjacency lists)
 */
#ifndef _DIGRAPH_H_
#define _DIGRAPH_H_

#include <vector>
#include <forward_list>
#include <cstddef>
#include <sstream>
#include <string>

using std::vector;
using std::forward_list;
using std::ostringstream;
using std::string;
using std::endl;

class Digraph {
  vector<forward_list<size_t>> vertices;

  public:
  explicit Digraph(size_t n_vertices);

  // TODO
  Digraph(const Digraph& that) = delete;
  Digraph(const Digraph&& that) = delete;
  Digraph& operator=(const Digraph& that) = delete;
  Digraph& operator=(const Digraph&& that) = delete;

  void connect(size_t from, size_t to);
  int n_vertices() const;
  forward_list<size_t> connected(size_t vertex) const;
  string to_s() const;
};

class Dfs {
  vector<bool> visited;
  vector<size_t> parent;
  int covered = 0;
  const Digraph& digraph;

  public:
  Dfs(const Digraph& _digraph);

  // TODO
  Dfs(const Dfs& that) = delete;
  Dfs& operator=(const Dfs& that) = delete;

  void dfs(size_t vertex);
  const vector<size_t> parents() const;
  int covered_vertices() const;
};

#endif
