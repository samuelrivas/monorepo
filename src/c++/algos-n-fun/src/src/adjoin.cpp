// Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>
#include <unordered_set>
#include <cmath>
#include <algorithm>
#include <vector>
#include <tuple>

#include "lib/graph.hpp"

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::get;
using std::make_tuple;
using std::max;
using std::move;
using std::tuple;
using std::unordered_set;

class FurthestCb : public BfsCallbacks {
  vector<int> hops;
  int max_hops = -1;
  int furthest = 0;
  unordered_set<int>* pending;

 public:
  FurthestCb(const Graph& _graph, unordered_set<int>* _pending) :
    hops(_graph.n_vertices(), -1),
    pending { _pending }
  { }

  void on_exit(int vertex, const vector<int>& parent,
               const vector<State>& state) {
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

  void reset() {
    max_hops = -1;
    furthest = 0;
  }
};

/* Run a first search to find the first end of each diameter */
vector<int> first_pass(const Graph& graph) {
  unordered_set<int> pending_vertices;

  for (int i = 0; i < graph.n_vertices(); i++) {
    pending_vertices.insert(i);
  }

  FurthestCb cb(graph, &pending_vertices);
  Bfs bfs(graph, &cb);
  vector<int> extreme_a;

  while (pending_vertices.size() > 0) {
    cb.reset();
    int vertex = *(pending_vertices.begin());
    bfs.bfs(vertex);
    int furthest = cb.get_furthest();
    extreme_a.push_back(furthest);
    cerr << "extreme a: " << furthest << endl;
  }
  return extreme_a;
}

tuple<int, int, int> second_pass(const Graph& graph,
                                 const vector<int>& extremes) {
  unordered_set<int> pending_vertices;
  FurthestCb cb(graph, &pending_vertices);
  Bfs bfs(graph, &cb);

  int largest = 0;
  int second_largest = 0;
  int third_largest = 0;

  for (int extreme : extremes) {
    cb.reset();
    bfs.bfs(extreme);
    int other_extreme = cb.get_furthest();
    int diameter = cb.get_distance();
    cerr << "diameter " << diameter << " from " << extreme
         << " to " << other_extreme << endl;

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
  return std::make_tuple(largest, second_largest, third_largest);
}

int main(void) {
  int n_computers, n_cables;

  cin.sync_with_stdio(false);
  cout.sync_with_stdio(false);

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

  // Get one extreme of the diameter for each connected component
  vector<int> extremes = first_pass(graph);

  // Get the three largest diameters
  tuple<int, int, int> diameters = second_pass(graph, extremes);
  int largest = get<0>(diameters);
  int second_largest = get<1>(diameters);
  int third_largest = get<2>(diameters);

  cerr << "1: " << largest << endl;
  cerr << "2: " << second_largest << endl;
  cerr << "3: " << third_largest << endl;

  /* We will connect all connected components in a star pattern, placing the
     one with the largest diameter in the center. The connections link a node in
     the center of the diameter.

     There are three cases:
     1 - the largest diameter dominates any other distance
     2 - the largest and second largest diameters, plus the hop between then
         dominate any other distance
     3 - the second an third largest diameters, plus the two links between them
         dominate any other distance

     3 covers the case when we have at least 3 connected components with the
     same diameter. The best possible way to link them is to put one in the
     center and connect the other two to it. The longest path is them to travel
     from one extreme of one of the components that is not the central one, to
     the extreme of the other component that isn't the central either.

     As long as we add components that have the same or less diameter, the
     response will not change (connecting them to the central component doesn't
     change the maximum distance. If we add a component with larger diameter, we
     move to either case 2 or 1.
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
