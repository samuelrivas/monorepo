#include <iostream>

#include "lib/digraph.hpp"

using std::cout;
using std::endl;

int main(void) {

  Digraph g(5);

  g.connect(0, 1);
  g.connect(0, 2);
  g.connect(0, 3);
  g.connect(0, 4);

  g.connect(4, 3);

  cout << g.to_s() << endl;

  Dfs dfs(g);
  dfs.dfs(0);
  return 0;
}
