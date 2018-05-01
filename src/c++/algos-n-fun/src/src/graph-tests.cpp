#include <forward_list>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>

#include "lib/digraph.hpp"

using std::forward_list;
using std::ostringstream;
using std::string;
using std::endl;
using std::cout;

class Graph {
  Digraph digraph;

 public:
  explicit Graph(int n_vertices) :
    digraph(n_vertices) { }

  void connect(int a, int b) {
    digraph.connect(a, b);
    digraph.connect(b, a);
  }

  int n_vertices() const {
    return digraph.n_vertices();
  }

  forward_list<int> connected(int vertex) const {
    return digraph.connected(vertex);
  }

  bool connected(int x, int y) {
    return digraph.connected(x, y);
  }

  string to_s() const {
    vector<bool> shown(n_vertices(), false);
    ostringstream out;

    for (int vertex = 0; vertex < n_vertices(); vertex++) {
      if (!shown[vertex]) {
        shown[vertex] = true;
        for (int x : connected(vertex)) {
          if (!shown[x]) {
            out << vertex << " -- " << x << endl;
          }
        }
      }
    }
    return out.str();
  }
};

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

  cout << graph.to_s();

  return 0;
}
