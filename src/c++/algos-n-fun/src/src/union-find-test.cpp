// Copyright (C) 2018 by samuelrivas@gmail.com

#include <vector>
#include <iostream>
#include <iomanip>
#include <string>

#include "lib/union-find.hpp"

using std::cout;
using std::endl;
using std::setw;
using std::string;

int main(void) {
  UnionFind uf(10);

  uf.join(1, 2);
  uf.join(1, 3);
  uf.join(3, 2);
  uf.join(5, 6);
  uf.join(1, 6);

  for (int x = 9; x >= 0; x--) {
    cout << x;
    for (int y = 0; y < 10; y++) {
      string mark = uf.joint(x, y) ? " X" : "  ";
      cout << mark;
    }
    cout << endl;
  }
  cout << " ";
  for (int y = 0; y < 10; y++) {
    cout << " " << y;
  }
  cout << endl;

  uf.join(1, 6);
  return 0;
}
