#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <list>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <regex>
#include <exception>
#include <limits>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::list;
using std::vector;
using std::pair;
using std::ostringstream;
using std::istringstream;
using std::string;
using std::setw;
using std::max;
using std::min;
using std::getline;
using std::regex;
using std::smatch;
using std::regex_match;
using std::terminate;
using std::numeric_limits;

typedef struct Coord {
  int x;
  int y;
} Coord;

string format_coord(const Coord& c) {
  ostringstream out;
  out << "(" << c.x << "," << c.y << ")";
  return out.str();
}

Coord sum_coord(const Coord& a, const Coord& b) {
  return { a.x + b.x, a.y + b.y };
}

int power_level(const Coord& c, int serial) {
  int rack_id = c.x + 11;
  int power = rack_id * (c.y + 1);
  power += serial;
  power *= rack_id;
  power = (power % 1000) / 100;
  power -= 5;
  return power;
}

int grid_power(const Coord& c, int size, int serial) {
  int power = 0;
  for (int x = 0; x < size; x++) {
    for (int y = 0; y < size; y++) {
      power += power_level(sum_coord(c, {x, y}), serial);
    }
  }
  return power;
}

int main(int argc, char *argv[]) {
  int serial;
  if (argc != 2) {
    // Convenience
    serial = 18;
  } else {
    istringstream sserial(argv[1]);
    sserial >> serial;
  }

  Coord max_coord;
  int max_power = numeric_limits<int>::min();

  // First part
  for (int x = 0; x < 297; x++) {
    for (int y = 0; y < 297; y++) {
      int power = grid_power({x, y}, 3, serial);
      if (power > max_power) {
        max_power = power;
        max_coord = { x, y };
      }
    }
  }

  Coord sol = sum_coord({1, 1}, max_coord);
  cout << "Solution 1: " << sol.x << "," << sol.y << endl;
  cout << "Power: " << max_power << endl;

  // Second part
  max_power = numeric_limits<int>::min();
  int max_size = numeric_limits<int>::min();

  for (int size = 1; size <= 300; size++) {
    cerr << "Testing for size: " << size << endl;
    for (int x = 0; x < 300 - size; x++) {
      int power = grid_power({x, 0}, size, serial);
      if (power > max_power) {
        max_size = size;
        max_power = power;
        max_coord = { x, 0 };
      }

      for (int y = 1; y < 300 - size; y++) {
        // Ugly optimisation as just calculating every convolution independently
        // is too slow. We are only calculating vertical strides incrementally
        // and still resetting for each x but this is fast enough
        int out_power = 0;
        int in_power = 0;
        for (int x_stride = 0; x_stride < size; x_stride++) {
          out_power += power_level({x + x_stride, y - 1}, serial);
          in_power += power_level({x + x_stride, y + size - 1}, serial);
        }
        power += in_power - out_power;

        if (power > max_power) {
          max_size = size;
          max_power = power;
          max_coord = { x, y };
        }
      }
    }
    cerr << "Current power: " << max_power << endl;
  }

  sol = sum_coord({1, 1}, max_coord);
  cout << "Solution 2: " << sol.x << "," << sol.y << "," << max_size << endl;
  cout << "Power: " << max_power << endl;

  return 0;
}
