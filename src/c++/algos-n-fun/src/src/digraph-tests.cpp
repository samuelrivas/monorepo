#include <iostream>

#include "lib/digraph.hpp"

using std::cout;
using std::endl;

int main(void) {

  Digraph g(5);

  g.connect(0, 1);
  g.connect(0, 2);
  g.connect(1, 2);
  g.connect(2, 3);
  g.connect(4, 5);

  cout << g.to_s() << endl;
  return 0;
}
