// Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>
#include <iomanip>
#include <vector>

#include "union-find.hpp"

using std::vector;
using std::cerr;
using std::endl;
using std::setw;

int UnionFind::find(int element) {
  if (parent[element] == element) {
    return element;
  } else {
    int new_root = find(parent[element]);
    if (new_root != parent[element]) {
      size[parent[element]] -= size[element];
      parent[element] = new_root;
    }
    return new_root;
  }
}

// For debugging
void UnionFind::print_state() const {
  cerr << "     ";
  for (size_t i= 0; i < size.size(); i++) {
    cerr << setw(3) << i;
  }
  cerr << endl;
  cerr << "root:";
  for (int x : parent) {
    cerr << setw(3) << x;
  }
  cerr << endl;
  cerr << "size:";
  for (int x : size) {
    cerr << setw(3) << x;
  }
  cerr << endl;
}

UnionFind::UnionFind(int elements) :
  parent(elements),
  size(elements, 1) {
  for (int i = 0; i < elements; i++) {
    parent[i] = i;
  }
}


void UnionFind::join(int x, int y) {
  // cerr << "join(" << x << "," << y << ")" << endl;
  // print_state();
  int root_x = find(x);
  int root_y = find(y);

  if (root_x == root_y) {
    return;
  }
  int size_root_x = size[root_x];
  int size_root_y = size[root_y];

  if (size_root_x > size_root_y) {
    parent[root_y] = root_x;
    size[root_x] += size_root_y;
  } else {
    parent[root_x] = root_y;
    size[root_y] += size_root_x;
  }
  // cerr << "after join" << endl;
  // print_state();
}

bool UnionFind::joint(int x, int y) {
  return find(x) == find(y);
}
