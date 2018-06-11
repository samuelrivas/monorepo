// Copyright (C) 2018 by samuelrivas@gmail.com
#include <iostream>
#include <vector>

#include "lib/digraph.hpp"

using std::cout;
using std::endl;

struct Callbacks : public DfsCallbacks {
  void on_edge(int from, int to,
               const vector<int>& parent,
               const vector<State>& state) override {
    (void) parent;

    if (state[to] == State::Processing) {
      std::cout << "and this is a cycle!" << std::endl;
      std::cout << to << " <- ";
      for (int offender = from; offender != to; offender = parent[offender]) {
        std::cout << offender << " <- ";
      }
      std::cout << to << std::endl;
    } else if (state[to] == State::Processed) {
      std::cout << "There was a cross link from " << from
                << " to " << to << std::endl;
    }
  }
  void on_exit(int vertex,
               const vector<int>& parent,
               const vector<State>& state) override {
    (void) parent;
    (void) state;

    std::cout << "covered: " << vertex << std::endl;
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
