#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <algorithm>
#include <limits>
#include <sstream>
#include <iomanip>
#include <unordered_set>
#include <cmath>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::pair;
using std::max_element;
using std::max;
using std::numeric_limits;
using std::ostringstream;
using std::setw;
using std::unordered_set;

typedef vector<vector<int>> Table;
typedef pair<int, int> Coord;

bool x_comp(Coord x, Coord y) {
  return x.first < y.first;
}

bool y_comp(Coord x, Coord y) {
  return x.second < y.second;
}

int distance(Coord x, Coord y) {
  return abs(x.first - y.first) + abs(x.second - y.second);
}

Coord get_max_coords(const vector<Coord>& coords) {
  return { max_element(coords.begin(), coords.end(), x_comp) -> first,
           max_element(coords.begin(), coords.end(), y_comp) -> second
  };
}

int read_table(const Table& t, const Coord& c) {
  return t[c.first][c.second];
}

void set_table(Table *t, const Coord& c, int x) {
  (*t)[c.first][c.second] = x;
}

string format_table(Table t) {
  ostringstream out;
  for (size_t y = 0; y < t[0].size(); y++) {
    for (size_t x = 0; x < t.size(); x++) {
      char c;
      switch (t[x][y]) {
      case 0:
        c = '.';
        break;
      case 1:
        c = '#';
        break;
      default:
        c = '?';
      }
      out << c;
    }
    out << endl;
  }
  return out.str();
}

int main(void) {
  vector<Coord> test_coords
    {
     {268, 273},
     {211, 325},
     {320, 225},
     {320, 207},
     {109, 222},
     {267, 283},
     {119, 70},
     {138, 277},
     {202, 177},
     {251, 233},
     {305, 107},
     {230, 279},
     {243, 137},
     {74, 109},
     {56, 106},
     {258, 97},
     {248, 346},
     {71, 199},
     {332, 215},
     {208, 292},
     {154, 80},
     {74, 256},
     {325, 305},
     {174, 133},
     {148, 51},
     {112, 71},
     {243, 202},
     {136, 237},
     {227, 90},
     {191, 145},
     {345, 133},
     {340, 299},
     {322, 256},
     {86, 323},
     {341, 310},
     {342, 221},
     {50, 172},
     {284, 160},
     {267, 142},
     {244, 153},
     {131, 147},
     {245, 323},
     {42, 241},
     {90, 207},
     {245, 167},
     {335, 106},
     {299, 158},
     {181, 186},
     {349, 286},
     {327, 108}
    };
    // {
    //  {1, 1},
    //  {1, 6},
    //  {8, 3},
    //  {3, 4},
    //  {5, 5},
    //  {8, 9}
    // };

  const int max_distance = 10000;
  // const int max_distance = 32;
  Coord max_coords = get_max_coords(test_coords);

  // Don't do this at home, but: -1 unknown, 0 unsafe, 1 safe
  // Also, we don't need this, it is just for debugging
  Table safe(max_coords.first + 1, vector<int>(max_coords.second + 1, -1));
  int safe_area = 0;

  for (int x = 0; x <= max_coords.first; x++) {
    for (int y = 0; y <= max_coords.second; y++) {
      int total_distance = 0;
      for (size_t k = 0;
           k < test_coords.size() && total_distance < max_distance;
           k++) {
        total_distance += distance({x, y}, test_coords[k]);
      }
      if (total_distance < max_distance) {
        // cerr << "(" << x << "," << y << ") is safe!" << endl;
        safe_area++;
      }
      set_table(&safe, {x, y}, total_distance < max_distance);
    }
  }

  cerr << format_table(safe) << endl;
  cout << "Solution: " << safe_area << endl;
  return 0;
}
