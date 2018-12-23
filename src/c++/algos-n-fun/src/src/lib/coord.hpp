#ifndef __COORD_HPP__
#define __COORD_HPP__

#include <sstream>
#include <string>

using std::ostringstream;
using std::string;

// Just 2d for now, we can generalise later if needed
template<typename T = int>
struct Coord {
  T x;
  T y;
  bool operator==(const Coord<T>& other) const {
    return other.x == x && other.y == y;
  }
  bool operator!=(const Coord<T>& other) const {
    return !operator==(other);
  }
};

template<typename T = int>
string format_coord(const Coord<T>& c) {
  ostringstream out;
  out << "(" << c.x << "," << c.y << ")";
  return out.str();
}

template<typename T = int>
Coord<T> sum_coord(const Coord<T>& a, const Coord<T>& b) {
  return { a.x + b.x, a.y + b.y };
}

#endif
