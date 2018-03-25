// Copyright (C) 2018 by samuelrivas@gmail.com
#include <vector>
#include <utility>
#include <iostream>

using std::vector;
using std::pair;
using std::cout;
using std::endl;

typedef vector<vector<bool>> Map;

void print_path(const vector<pair<int, int>>& path) {
  for (pair<int, int> coord : path) {
    cout << " (" << coord.first << "," << coord.second << ")";
  }
  cout << endl;
}

void print_paths(const Map& map, const pair<int, int>& coord,
                 const vector<pair<int, int>>& path) {
  int x = coord.first;
  int y = coord.second;
  int x_size = map.size();
  int y_size = map[0].size();  // assuming well behaved maps

  if (x >= x_size || y >= y_size || !map[x][y]) {
    // we've hit a wall here
    return;
  }

  if (x == x_size - 1 && y == y_size - 1) {
    // this is a solution
    print_path(path);
    return;
  }

  // Explore right
  vector<pair<int, int>> path_right(path);
  pair<int, int> coord_right(x, y + 1);
  path_right.push_back(coord_right);
  print_paths(map, coord_right, path_right);

  // Explore down
  vector<pair<int, int>> path_down(path);
  pair<int, int> coord_down(x + 1, y);
  path_down.push_back(coord_down);
  print_paths(map, coord_down, path_down);
}

void print_paths(const Map& map) {
  pair<int, int> initial_coord {0, 0};
  vector<pair<int, int>> initial_path { initial_coord };
  print_paths(map, initial_coord, initial_path);
}

int main(void) {
  Map map = {
    {true, true, true},
    {true, true, false},
    {true, true, true},
    {true, false, true},
    {true, true, true}
  };

  print_paths(map);
  cout << endl;

  return 0;
}
