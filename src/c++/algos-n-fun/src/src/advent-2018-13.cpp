#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>

#include "lib/circular-array.hpp"
#include "lib/coord.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::pair;
using std::ostringstream;
using std::istringstream;
using std::string;
using std::max;
using std::min;
using std::getline;
using std::terminate;
using std::numeric_limits;
using std::hex;
using std::find;
using std::set;

typedef vector<vector<char>> Map;
const vector<char> cart_shapes { '>', 'v', '<', '^' }; // clockwise

char read_map(const Map& map, const Coord<>& coord) {
  return map[coord.y][coord.x];
}

Coord<> cart_to_delta_coord(char c) {
  switch (c) {
  case '>':
    return { 1, 0 };
  case '<':
    return { -1, 0 };
  case 'v':
    return { 0, 1 };
  case '^':
    return { 0, -1 };
  default:
    cerr << "This is not a cart: " << c << endl;
    abort();
  }
}

class Cart {
private:
  CircularArray<char> shapes { cart_shapes }; // advancing turns clockwise
  CircularArray<int> turns { -1, 0, 1 }; // counter, nothing, and clockwise

  void turn() {
    int delta = turns.get_and_fw();
    shapes.increment_pos(delta);
  }

  void curve(char track) {
    int increment;
    int mult = track == '/' ? 1 : -1;
    switch (shape()) {
    case '<':
    case '>':
      increment = -1 * mult;
      break;
    case '^':
    case 'v':
      increment = 1 * mult;
      break;
    default:
      cerr << "This is not a cart: " << shape() << endl;
      abort();
    }
    shapes.increment_pos(increment);
  }

public:
  Coord<> pos;

  Cart(char direction, Coord<> _pos) :
    pos { _pos } {
    int count = 0;
    while (shapes.get() != direction) {
      shapes.fw();
      count++;
      assert(count < 4);
    }
  }

  char shape() const {
    return shapes.get();
  }

  Coord<> advance(const Map& map) {
    pos = sum_coord(cart_to_delta_coord(shape()), pos);
    land(map);
    return pos;
  }

private:
  void land(const Map& map) {
    char terrain = read_map(map, pos);
    switch (terrain) {
    case '/':
    case '\\':
      curve(terrain);
      return;
    case '+':
      turn();
      return;
    case '|':
      assert(shape() == '^' || shape() == 'v');
      return;
    case '-':
      assert(shape() == '>' || shape() == '<');
      return;
    default:
      cerr << "Are we off piste now? " << terrain << endl;
      abort();
    }
  }
};

struct CartComp {
  bool operator()(const Cart& a, const Cart& b) const {
    return (a.pos.y < b.pos.y)
      || ((a.pos.y == b.pos.y) && (a.pos.x < b.pos.x));
  }
};

typedef set<Cart,CartComp> Carts;

char cart_to_track(char c) {
  switch (c) {
  case '>':
  case '<':
    return '-';
  case 'v':
  case '^':
    return '|';
  default:
    cerr << "This is not a cart: " << c << endl;
    abort();
  }
}

pair<Map, Carts> parse_map(const vector<string>& input) {
  Map map;
  Carts carts;

  for (int y = 0; y < static_cast<int>(input.size()); y++) {
    vector<char> map_line;

    for (int x = 0; x < static_cast<int>(input[y].size()); x++) {
      char c = input[y][x];
      if (find(cart_shapes.begin(), cart_shapes.end(), c)
          != cart_shapes.end()) {

        carts.insert(Cart(c, { x, y }));
        c = cart_to_track(c);
      }

      map_line.push_back(c);
    }
    map.push_back(map_line);
  }
  return { map, carts };
}

string format_map(const Map& map, const Carts& carts) {
  ostringstream out;
  for (int y = 0; y < static_cast<int>(map.size()); y++) {
    for (int x = 0; x < static_cast<int>(map[y].size()); x++) {

      // Abusing set a bit here, knowing that we don't use the shape for sorting
      Carts::const_iterator it = carts.find({ '>', {x, y} });
      if (it != carts.end()) {
        out << (*it).shape();
      } else {
        out << map[y][x];
      }
    }
    out << endl;
  }
  return out.str();
}

int main(void) {
  cin.sync_with_stdio(false);

  vector<string> input_lines;
  for (string line; getline(cin, line);) {
    input_lines.push_back(line);
  }

  Map map;
  Carts carts;
  {
    pair<Map, Carts> parsed = parse_map(input_lines);
    map = parsed.first;
    carts = parsed.second;
  }

  cerr << format_map(map, carts);

  while (true) {
    Carts new_carts;
    while (carts.begin() != carts.end()) {
      Cart cart = *(carts.begin());
      carts.erase(carts.begin());

      cerr << "Advancing cart " << cart.shape()
           << " in " << format_coord(cart.pos);

      cart.advance(map);

      cerr << " to " << format_coord(cart.pos)
           << " as " << cart.shape() << endl;

      if (carts.find(cart) != carts.end()
          || new_carts.find(cart) != new_carts.end()) {
        // Collision!
        cout << "Solution: " << format_coord(cart.pos) << endl;
        return 0;
      }

      new_carts.insert(cart);
    }
    carts = new_carts;
    cerr << format_map(map, carts);
  }
  return 0;
}
