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
  vector<forward_list<int>> vertices;

  public:
  explicit Digraph(int n_vertices);

  void connect(int from, int to);
  int n_vertices() const;
  forward_list<int> connected(int vertex) const;
  string to_s() const;
};

enum class State {
  Unprocessed,
  Processing,
  Processed
};

/* on_entry is called when we reach a vertex, before any of its descendants are
   processed.

   on_exit is called when all the descendants have been processed.

   on_edge is called once per edge, when processing a node.

   Note that "descendant" means unprocessed, connected node, which depends on
   the traversal order

   We do not offer any form of early termination as this will either complicate
   the code significantly, or make the semantics of the state array and the
   callbacks unclear
*/
struct DfsCallbacks {
  virtual void on_entry(int vertex,
                        const vector<int>& parent,
                        const vector<State>& state);

  virtual void on_exit(int vertex,
                       const vector<int>& parent,
                       const vector<State>& state);
  virtual void on_edge(int from, int to,
                       const vector<int>& parent,
                       const vector<State>& state);
};

class Dfs {
  vector<bool> visited;
  vector<bool> processed;
  vector<State> state;
  vector<int> parent;
  DfsCallbacks* callbacks;
  const Digraph& digraph;

  public:
  Dfs(const Digraph& _digraph, DfsCallbacks* _callbacks);

  // TODO
  Dfs(const Dfs& that) = delete;
  Dfs& operator=(const Dfs& that) = delete;

  void dfs(int vertex);
  const vector<int> parents() const;
};

#endif
