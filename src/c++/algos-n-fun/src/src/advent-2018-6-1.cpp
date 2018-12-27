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
      if (t[x][y] >= 0) {
        out << setw(3) << t[x][y];
      } else {
        out << setw(3) << (t[x][y] == -1 ? 'o' : '.');
      }
    }
    out << endl;
  }
  return out.str();
}

struct Expansion_set_hash {
  inline size_t operator()(const pair<int,Coord>& x) const {
    return 17 + x.first*31 + x.second.first*37 + x.second.second*23;
  }
};

typedef unordered_set<pair<int,Coord>, Expansion_set_hash> Expansion_set;

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

  Coord max_coords = get_max_coords(test_coords);

  // -1 unclaimed, -2 neutral, other claimed
  Table claims(max_coords.first + 1, vector<int>(max_coords.second + 1, -1));

  // Distance to the centroid, max_int if unknown
  Table distances(max_coords.first + 1, vector<int>(max_coords.second + 1,
                                             numeric_limits<int>::max()));
  vector<int>areas(test_coords.size(), 0);
  unordered_set<int> feasible_centroids;

  int current_distance { 0 };
  Expansion_set to_expand;
  for (size_t i = 0; i < test_coords.size(); i++) {
    to_expand.insert({i, test_coords[i]});
    feasible_centroids.insert(i);
  }

  while (! to_expand.empty()) {
    Expansion_set to_expand_next;
    assert(to_expand_next.empty());

    for (pair<int,Coord> c : to_expand) {

      cerr << "Expanding " << c.first << " to ("
           << c.second.first << "," << c.second.second
           << "), distance " << current_distance << endl;

      // Claim area
      if (read_table(distances, c.second) < current_distance) {
        // claimed before, do nothing
        cerr << "Already taken by " << read_table(claims, c.second)
             << " with distance " << read_table(distances, c.second) << endl;
      } else if (read_table(distances, c.second) == current_distance) {
        // tie, set it as dmz
        cerr << "Tied with with " << read_table(claims, c.second)
             << " with distance " << read_table(distances, c.second) << endl;
        assert(read_table(claims, c.second) != c.first);

        if (read_table(claims, c.second) >= 0) {
          areas[read_table(claims, c.second)]--;
        }
        set_table(&claims, c.second, -2);
      } else {
        // Claim this and set more expansion cells
        cerr << "Free, expanding" << endl;
        assert(read_table(distances, c.second) == numeric_limits<int>::max());
        set_table(&distances, c.second, current_distance);
        set_table(&claims, c.second, c.first);
        areas[c.first]++;
      }

      // Set next expansion coords, if we haven't hit a cell closer to another
      // centroid
      if (read_table(distances, c.second) == current_distance) {
        vector<Coord> candidates
          {
           {c.second.first - 1, c.second.second},
           {c.second.first + 1, c.second.second},
           {c.second.first, c.second.second - 1},
           {c.second.first, c.second.second + 1}
          };
        for (Coord candidate : candidates) {
          if (candidate.first >= 0
              && candidate.first <= max_coords.first
              && candidate.second >= 0
              && candidate.second <= max_coords.second
              && read_table(claims, candidate) == -1) {
            to_expand_next.insert({c.first, candidate});
          }
        }
      }
    }

    cerr << format_table(claims) << endl;
    current_distance++;
    to_expand = to_expand_next;
  }

  for (int x = 0; x <= max_coords.first; x++) {
    int centroid = read_table(claims, {x, 0});
    if (centroid >= 0) {
      cerr << centroid << " touched fire" << endl;
      feasible_centroids.erase(centroid);
    }
    centroid = read_table(claims, {x, max_coords.second});
    if (centroid >= 0) {
      cerr << centroid << " touched fire" << endl;
      feasible_centroids.erase(centroid);
    }
  }
  for (int y = 0; y <= max_coords.second; y++) {
    int centroid = read_table(claims, {0, y});
    if (centroid >= 0) {
      cerr << centroid << " touched fire" << endl;
      feasible_centroids.erase(centroid);
    }
    centroid = read_table(claims, {max_coords.first, y});
    if (centroid >= 0) {
      cerr << centroid << " touched fire" << endl;
      feasible_centroids.erase(centroid);
    }
  }

  int max_area = 0;
  for (size_t i = 0; i < areas.size(); i++) {
    if (feasible_centroids.find(i) != feasible_centroids.end()) {
      cerr << "area " << i << " size " << areas[i] << endl;
      max_area = max(max_area, areas[i]);
    }
  }

  cout << "Solution: " << max_area << endl;
  return 0;
}
