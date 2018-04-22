#include <iostream>
#include <string>
#include <array>
#include <cassert>

#include "lib/union-find.hpp"

using std::cout;
using std::cin;
using std::string;
using std::array;
using std::endl;
using std::cerr;

int translate(int x, int y, int size_x) {
  return y * size_x + x;
}

void print_reachability(UnionFind* uf_binary, UnionFind* uf_decimal, int source, int dest, const vector<bool>& binary_positions) {
  if (source == dest) {
    // Cannot use our union finds here
    if (binary_positions[source]) {
      cout << "binary" << endl;
    } else {
      cout << "decimal" << endl;
    }
    return;
  }

  bool binary = uf_binary -> joint(source, dest);
  bool decimal = uf_decimal -> joint(source, dest);

  assert(!binary || !decimal);

  if (binary) {
    cout << "binary" << endl;
    return;
  }
  if (decimal) {
    cout << "decimal" << endl;
    return;
  }
  cout << "neither" << endl;
}

int main(void) {
  cin.sync_with_stdio(false);
  cout.sync_with_stdio(false);

  int size_x, size_y;

  cin >> size_y;
  cin >> size_x;

  array<UnionFind, 2> uf {
    UnionFind(size_x * size_y),
    UnionFind(size_x * size_y)
  };

  vector<bool> binary_positions(size_x * size_y, false);
  string previous_row;
  for (int y = 0; y < size_y; y++) {
    string row;
    cin >> row;

    for (int x = 0; x < size_x; x++) {
      if (row[x] == '0') {
        binary_positions[translate(x, y, size_x)] = true;
      }
      if (y > 0) {
        if (row[x] == previous_row[x]) {
          uf[row[x] - '0'].join(translate(x, y, size_x), translate(x , y - 1, size_x));
        }
      }
      if (x > 0) {
        if (row[x] == row[x - 1]) {
          uf[row[x] - '0'].join(translate(x, y, size_x), translate (x - 1, y, size_x));
        }
      }
    }
    previous_row = row;
  }
  // cerr << "For 0s:" << endl;
  // uf[0].print_state();
  // cerr << "For 1s:" << endl;
  // uf[1].print_state();

  int checks;
  cin >> checks;

  for (int check = 0; check < checks; check++) {
    int x1, y1, x2, y2;
    cin >> y1;
    cin >> x1;
    cin >> y2;
    cin >> x2;

    int source = translate(x1 - 1, y1 - 1, size_x);
    int dest = translate(x2 - 1, y2 - 1, size_x);

    print_reachability(&uf[0], &uf[1], source, dest, binary_positions);
  }
  return 0;
}
