#include <iostream>
#include <utility>
#include <set>
#include <cmath>

#include "lib/graph.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::pair;
using std::get;
using std::move;
using std::get;
using std::set;
using std::max;

class FurthestCb : public BfsCallbacks {
  vector<int> hops;
  int max_hops = -1;
  int furthest = 0;
  set<int>* pending;

 public:
  FurthestCb(const Graph& _graph, set<int>* _pending) :
    hops(_graph.n_vertices(), -1),
    pending { _pending }
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
    pending -> erase(vertex);
  }

  int get_furthest() const {
    return furthest;
  }
  int get_distance() const {
    return max_hops;
  }
};

// {furthest, distance}
pair<int, int> furthest_from(const Graph& graph, int vertex,
                             set<int> *pending) {
  FurthestCb cb(graph, pending);
  Bfs bfs(graph, &cb);
  bfs.bfs(vertex);

  return {cb.get_furthest(), cb.get_distance()};
}

int main(void) {

  int n_computers, n_cables;
  cin >> n_computers;
  cin >> n_cables;

  Graph graph(n_computers);

  for (int i = 0; i < n_cables; i++) {
    int from, to;
    cin >> from;
    cin >> to;
    graph.connect(from, to);
  }

  cerr << graph.to_s();

  set<int> pending_vertices;

  for (int i = 0; i < graph.n_vertices(); i++) {
    pending_vertices.insert(i);
  }

  int largest = 0;
  int second_largest = 0;
  int third_largest = 0;

  while (pending_vertices.size() > 0) {
    int vertex = *(pending_vertices.begin());
    pair<int, int> extreme_a = furthest_from(graph, vertex, &pending_vertices);
    pair<int, int> extreme_b = furthest_from(graph, extreme_a.first,
                                             &pending_vertices);

    int diameter = extreme_b.second;

    cerr << "Diameter: " << diameter << ". From " << extreme_a.first
         << " to " << extreme_b.first << endl;

    if (diameter > largest) {
      third_largest = second_largest;
      second_largest = largest;
      largest = diameter;
    } else if (diameter > second_largest) {
      third_largest = second_largest;
      second_largest = diameter;
    } else if (diameter > third_largest) {
      third_largest = diameter;
    }
  }

  // cerr << "1: " << largest << endl;
  // cerr << "2: " << second_largest << endl;

  /* We will connect all connected components in a star pattern, placing the
     one with the largest diameter in the center.

     There are three cases:
     1 - the largest diameter dominates any other distance
     2 - the largest and second largest diameters, plus the hop between then
         dominate any other distance
     3 - the second an third largest diameters, plus the two links between them
         dominate any other distance
  */
  int sol = max(largest,
                static_cast<int>(max(
                                     ceil(largest/2.0)
                                     + ceil(second_largest/2.0)
                                     + 1,

                                     ceil(second_largest/2.0)
                                     + ceil(third_largest/2.0)
                                     +2)));
  cout << sol << endl;;
  return 0;
}
