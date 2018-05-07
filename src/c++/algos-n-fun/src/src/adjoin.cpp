#include <iostream>
#include <tuple>
#include <utility>

#include "lib/graph.hpp"

using std::cout;
using std::endl;
using std::tuple;
using std::get;
using std::move;
using std::get;

class FurthestCb : public BfsCallbacks {
  vector<int> hops;
  int max_hops = -1;
  int furthest = 0;
  vector<bool> visited;  // Todo this duplicates data from CB

 public:
  FurthestCb(const Graph& _graph) :
    hops(_graph.n_vertices(), -1),
    visited(_graph.n_vertices(), false)
  { }

  void on_exit(int vertex, const vector<int>& parent, const vector<State>& state) {
    (void) state;

    if (parent[vertex] == -1) {
      hops[vertex] = 0;
    } else {
      hops[vertex] = hops[parent[vertex]] + 1;
    }

    if (max_hops < hops[vertex]) {
      max_hops = hops[vertex];
      furthest = vertex;
    }
    visited[vertex] = true;
  }

  int get_furthest() const {
    return furthest;
  }
  int get_distance() const {
    return max_hops;
  }
  vector<bool> get_visited() {
    return visited;
  }
};

// {furthest, distance, visited}
tuple<int, int, vector<bool>> furthest_from(const Graph& graph, int vertex) {
  FurthestCb cb(graph);
  Bfs bfs(graph, &cb);
  bfs.bfs(vertex);

  return {cb.get_furthest(), cb.get_distance(), cb.get_visited()};
}

int main(void) {
  Graph graph(8);

  graph.connect(0, 2);
  graph.connect(5, 1);
  graph.connect(3, 1);
  graph.connect(3, 2);
  graph.connect(3, 4);
  graph.connect(6, 0);
  graph.connect(3, 7);


  tuple<int, int, vector<bool>> extreme_a = furthest_from(graph, 0);
  tuple<int, int, vector<bool>> extreme_b = furthest_from(graph, get<0>(extreme_a));

  int diameter = get<1>(extreme_b);

  cout << "Diameter: " << diameter << ". From " << get<0>(extreme_a) << " to " << get<0>(extreme_b) << endl;
}
