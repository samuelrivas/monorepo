#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>
#include <unordered_set>
#include <unordered_map>
#include <queue>

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
using std::unordered_set;
using std::unordered_map;
using std::queue;

struct CoordHasher {
  size_t operator()(const Coord<>& c) const {
    return 17 + c.x * 31 + c.y * 37;
  }
};

typedef vector<vector<char>> Map;
typedef unordered_set<Coord<>,CoordHasher> CoordSet;
typedef unordered_map<Coord<>,Coord<>,CoordHasher> CoordMap;

char read_map(const Map& map, const Coord<>& coord) {
  return map[coord.y][coord.x];
}

class Creature {
public:
  Coord<> pos;
  int hit_points = 300;
  char type;

  Creature(char _type, Coord<> _pos) :
    pos { _pos },
    type { _type } { }

  // Hit this creature
  void hit() {
    hit_points -= 3;
  }

  bool alive() const {
    return hit_points > 0;
  }
};

struct CreatureComp {
  bool operator()(const Creature& a, const Creature& b) const {
    return (a.pos.y < b.pos.y)
      || ((a.pos.y == b.pos.y) && (a.pos.x < b.pos.x));
  }
};

typedef set<Creature,CreatureComp> Creatures;

pair<Map,Creatures> parse_map(const vector<string>& input) {
  Map map;
  Creatures creatures;

  for (int y = 0; y < static_cast<int>(input.size()); y++) {
    vector<char> map_line;

    for (int x = 0; x < static_cast<int>(input[y].size()); x++) {
      char c = input[y][x];
      if (c == 'G' || c == 'E') {
        creatures.insert(Creature(c, { x, y }));
        c = '.';
      }
      map_line.push_back(c);
    }
    map.push_back(map_line);
  }
  return { map, creatures };
}

bool has_creature(const Creatures& creatures,
                  const Coord<>& coord, char type) {
  if (creatures.find(Creature('x', coord)) != creatures.end()) {
    Creature creature = *(creatures.find(Creature('x', coord)));
    return creature.type == type;
  }
  return false;
}

Coord<> next_move(const Map& map, const Creatures& creatures,
                const Creature& creature) {
  queue<Coord<>> q;

  q.push(creature.pos);
  bool found = false;
  CoordMap parents;
  char target_type = creature.type == 'E' ? 'G' : 'E';
  Coord<> expanding { -1 , -1 };

  cerr << "Starting search in " << format_coord(creature.pos)
       << " for a " << creature.type << endl;
  while (q.size() > 0) {
    expanding = q.front();
    cerr << "Expanding " << format_coord(expanding) << endl;
    q.pop();

    assert(expanding != creature.pos || has_creature(creatures, expanding, creature.type));

    if (has_creature(creatures, expanding, target_type)) {
      cerr << "Found a valid target in " << format_coord(expanding) << endl;
      found = true;
      break;
    }

    // Expand all adjacent, if they are empty cells
    vector<Coord<>> deltas {
      { 0 , -1 }, // up
      { -1,  0 }, // left
      {  1,  0 }, // right
      {  0,  1 }  // down
    };

    for (Coord<> delta : deltas) {
      Coord<> to_expand = sum_coord(expanding, delta);
      if (read_map(map, to_expand) == '.'
          && !has_creature(creatures, to_expand, creature.type)
          && parents.find(to_expand) == parents.end()) {
        cerr << "Adding to queue " << format_coord(to_expand) << endl;
        q.push(to_expand);
        parents[to_expand] = expanding;
      }
    }
  }

  if (found) {
    Coord<> parent = parents[expanding];
    Coord<> move = parent;
    cerr << "parent: " << format_coord(parent) << endl;
    while (!(parent == creature.pos)) {
      move = parent;
      parent = parents[parent];
      cerr << "parent: " << format_coord(parent) << endl;
    }
    cerr << "Final move: " << format_coord(move) << endl;
    return move;
  }
  return { -1, -1 };
}

string format_map(const Map& map, const Creatures& creatures) {
  ostringstream out;
  for (int y = 0; y < static_cast<int>(map.size()); y++) {
    for (int x = 0; x < static_cast<int>(map[y].size()); x++) {

      // Abusing set a bit here, knowing that we don't use the type for sorting
      Creatures::const_iterator it = creatures.find({ 'X', {x, y} });
      if (it != creatures.end()) {
        out << (*it).type;
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
  Creatures creatures;
  {
    pair<Map,Creatures> parsed = parse_map(input_lines);
    map = parsed.first;
    creatures = parsed.second;
  }

  Creature creature = *(creatures.begin());
  Coord<> move = next_move(map, creatures, creature);

  cerr << format_coord(move) << endl;
  cerr << format_map(map, creatures);

  return 0;
}
