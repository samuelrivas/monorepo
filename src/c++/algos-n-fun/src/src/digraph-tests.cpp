#include <iostream>

#include "lib/digraph.hpp"

using std::cout;
using std::endl;

struct Callbacks : public DfsCallbacks {
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
