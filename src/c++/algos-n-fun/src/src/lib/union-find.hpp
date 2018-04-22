// Copyright (C) 2018 by samuelrivas@gmail.com

#ifndef _UNION_FIND_H_
#define _UNION_FIND_H_

#include <vector>
#include <iostream>
#include <iomanip>
#include <string>

using std::vector;

class UnionFind {
  vector<int> parent;
  vector<int> size;

  int find(int element);
  void print_state() const;
 public:
  explicit UnionFind(int elements);

  // C++ won't let us call this "union"
  void join(int x, int y);

  bool joint(int x, int y);
};

#endif
