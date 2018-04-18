#include <iostream>

#include "lib/digraph.hpp"

using std::cout;
using std::endl;

struct Callbacks : public DfsCallbacks {
  virtual void on_processed(int vertex, const vector<int>& parent,
                            const vector<bool>& processed) const {
    (void) parent;
    (void) processed;

    cout << "PROCESSED: " << vertex << endl;
  }
  virtual void on_seen(int vertex, const vector<int>& parent,
                       const vector<bool>& processed) const {
    (void) vertex;
    (void) parent;
    (void) processed;

    cout << "SEEN: " << vertex << endl;
  }
} callbacks;


int main(void) {

  Digraph g(5);

  g.connect(0, 1);
  g.connect(0, 2);
  g.connect(0, 3);
  g.connect(0, 4);

  g.connect(4, 3);
  g.connect(3, 2);
  g.connect(2, 4);

  cout << g.to_s() << endl;

  Dfs dfs(g, &callbacks);
  dfs.dfs(0);
  return 0;
}
