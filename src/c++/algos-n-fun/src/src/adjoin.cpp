#include <iostream>
#include <utility>

#include "lib/graph.hpp"

using std::cout;
using std::endl;
using std::pair;

class FurthestCb : public BfsCallbacks {
  vector<int> hops;
  int max_hops = -1;
  int furthest = 0;

 public:
  FurthestCb(const Graph& _graph) : hops(_graph.n_vertices(), -1) { }

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
  }

  int get_furthest() {
    return furthest;
  }
  int get_distance() {
    return max_hops;
  }
};

pair<int, int> furthest_from(const Graph& graph, int vertex) {
  FurthestCb cb(graph);
  Bfs bfs(graph, &cb);
  bfs.bfs(vertex);

  return {cb.get_furthest(), cb.get_distance()};
}

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


  pair<int, int> extreme_a = furthest_from(graph, 0);
  pair<int, int> extreme_b = furthest_from(graph, extreme_a.first);

  int diameter = extreme_b.second;

  cout << "Diameter: " << diameter << ". From " << extreme_a.first << " to " << extreme_b.first << endl;
}
