/* The BFS implementation in this code has a couple of bugs, that don't seem to
   affect the solution for my input in the first part, nor any of the summarised
   examples for the second part, but does break for my input in the second
   part. If you happen to be interested on this solution, read the second part
   implementation.

   The first and obvious problem is that the nodes are potentially expanded in
   the wrong order, this example will have the Elf moving down instead of right:

######
#.E.G#
#G...#
######

  The second and less obvious problem is that, by tracking the target creature,
  and not the in-range cells, some very specific positions end up resolving
  with the wrong moves. Even with the bug above fixed, the next position breaks
  again:

#######
#...#G#
#..E..#
#G....#
#######

  With the Elf targeting the goblin above instead of the goblin below, and thus
  moving right, when the right move is left. This happens because the goblin
  above is in a more prioritised position, however the in-range cell, which is
  the one right below it is not.
*/

#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <queue>

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
using std::map;

struct CoordHasher {
  size_t operator()(const Coord<>& c) const {
    return 17 + c.x * 31 + c.y * 37;
  }
};

typedef vector<vector<char>> Map;
typedef unordered_set<Coord<>,CoordHasher> CoordSet;
typedef unordered_map<Coord<>,Coord<>,CoordHasher> CoordMap;

// Expand all adjacent, if they are empty cells
const vector<Coord<>> deltas {
  { 0 , -1 }, // up
  { -1,  0 }, // left
  {  1,  0 }, // right
  {  0,  1 }  // down
};

char read_map(const Map& map, const Coord<>& coord) {
  return map[coord.y][coord.x];
}

class Creature {
public:
  Coord<> pos;
  int hit_points = 200;
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

struct CoordComp {
  bool operator()(const Coord<>& a, const Coord<>& b) const {
    return (a.y < b.y)
      || ((a.y == b.y) && (a.x < b.x));
  }
};

typedef map<Coord<>,Creature,CoordComp> Creatures;

pair<Map,Creatures> parse_map(const vector<string>& input) {
  Map map;
  Creatures creatures;

  for (int y = 0; y < static_cast<int>(input.size()); y++) {
    vector<char> map_line;

    for (int x = 0; x < static_cast<int>(input[y].size()); x++) {
      char c = input[y][x];
      if (c == 'G' || c == 'E') {
        creatures.emplace(Coord<>({ x, y }), Creature(c, { x, y }));
        c = '.';
      }
      map_line.push_back(c);
    }
    map.push_back(map_line);
  }
  return { map, creatures };
}

string format_creature(const Creature& creature) {
  ostringstream out;

  out << creature.type << "(" << creature.hit_points << ") in "
      << format_coord(creature.pos);

  return out.str();
}

bool has_creature(const Creatures& creatures,
                  const Coord<>& coord, char type) {
  if (creatures.find(coord) != creatures.end()) {
    return creatures.at(coord).type == type;
  }
  return false;
}

char opposite(char type) {
  return type == 'E' ? 'G' : 'E';
}

Coord<> next_move(const Map& map, const Creatures& creatures,
                  const Creatures& new_creatures,
                  const Creature& creature) {
  queue<Coord<>> q;

  q.push(creature.pos);
  bool found = false;
  CoordMap parents;
  char target_type = opposite(creature.type);
  Coord<> expanding { -1 , -1 };

  // cerr << "Starting search in " << format_coord(creature.pos)
  //      << " for a " << creature.type << endl;
  while (q.size() > 0) {
    expanding = q.front();
    // cerr << "Expanding " << format_coord(expanding) << endl;
    q.pop();

    if (has_creature(creatures, expanding, target_type)
        || has_creature(new_creatures, expanding, target_type)) {
      // cerr << "Found a valid target in " << format_coord(expanding) << endl;
      found = true;
      break;
    }


    for (Coord<> delta : deltas) {
      Coord<> to_expand = sum_coord(expanding, delta);
      if (read_map(map, to_expand) == '.'
          && !has_creature(creatures, to_expand, creature.type)
          && !has_creature(new_creatures, to_expand, creature.type)
          && parents.find(to_expand) == parents.end()) {
        // cerr << "Adding to queue " << format_coord(to_expand) << endl;
        q.push(to_expand);
        parents[to_expand] = expanding;
      }
    }
  }

  if (found) {
    Coord<> parent = parents[expanding];
    Coord<> move = parent;
    // cerr << "parent: " << format_coord(parent) << endl;
    while (!(parent == creature.pos)) {
      move = parent;
      parent = parents[parent];
      // cerr << "parent: " << format_coord(parent) << endl;
    }
    // cerr << "Final move: " << format_coord(move) << endl;
    return move;
  }
  return { -1, -1 };
}

string format_map(const Map& map, const Creatures& creatures) {
  ostringstream out;
  for (int y = 0; y < static_cast<int>(map.size()); y++) {
    for (int x = 0; x < static_cast<int>(map[y].size()); x++) {
      if (creatures.find({ x, y }) != creatures.end()) {
        out << creatures.at({ x, y }).type;
      } else {
        out << map[y][x];
      }
    }
    out << endl;
  }
  return out.str();
}

// 0 no attack, 1 in  creatures, 2 in new_creatures
// ... ugly, yes
int has_attack(const Creature& creature,
               const Creatures& creatures,
               const Creatures& new_creatures,
               Coord<>* out) {
  int result = 0;
  int hp = numeric_limits<int>::max();
  for (Coord<> d : deltas) {
    Coord<> target_pos = sum_coord(creature.pos, d);

    if (has_creature(creatures, target_pos, opposite(creature.type))
        && creatures.at(target_pos).hit_points < hp) {
      hp = creatures.at(target_pos).hit_points;
      cerr << "Found feasible creature in creatures: "
           << format_creature(creatures.at(target_pos)) << endl;
      *out = target_pos;
      result = 1;
    }
    if (has_creature(new_creatures, target_pos, opposite(creature.type))
        && new_creatures.at(target_pos).hit_points < hp) {
      hp = new_creatures.at(target_pos).hit_points;
      cerr << "Found feasible creature in new_creatures: "
           << format_creature(new_creatures.at(target_pos)) << endl;
      *out = target_pos;
      result = 2;
    }
  }
  if (result) {
    cerr << "Attack: " << result << " " << format_coord(*out) << endl;
  }
  return result;
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

  int elves = 0;
  int goblins = 0;
  for (auto creature : creatures) {
    if (creature.second.type == 'E') {
      elves++;
    } else {
      goblins++;
    }
  }

  int rounds = 0;
  int continue_combat = true;
  while (continue_combat) {
    Creatures new_creatures;
    while (creatures.size() > 0) {
      Creature creature = creatures.begin() -> second;
      creatures.erase(creature.pos);

      cerr << "Processing " << creature.type
           << " in " << format_coord(creature.pos) << endl;;

      if ((creature.type == 'E' && goblins == 0)
          || (creature.type == 'G' && elves == 0)) {
        cerr << "No more enemies, finishing combat after this round" << endl;
        continue_combat = false;
      }

      Coord<> move = next_move(map, creatures, new_creatures, creature);

      if (move.x == -1) {
        cerr << "won't move!" << endl;
      } else {
        cerr << "Moving to " << format_coord(move) << endl;
        creature.pos = move;
      }
      new_creatures.emplace(creature.pos, creature);

      Coord<> attack_coord;
      int attack = has_attack(creature, creatures, new_creatures, &attack_coord);
      if (attack != 0) {
        Creature attacked('x', {-1, -1});
        if (attack == 1) {
          attacked = creatures.at(attack_coord);
          creatures.erase(attack_coord);
        } else if (attack == 2) {
          attacked = new_creatures.at(attack_coord);
          new_creatures.erase(attack_coord);
        }

        attacked.hit();
        cerr << "Attacked " << attacked.type
             << "(" << attacked.hit_points << ") in "
             << format_coord(attacked.pos) << endl;

        if (!attacked.alive()) {
          cerr << "And is dead!" << endl;
          if (attacked.type == 'E') {
            elves--;
          } else {
            goblins--;
          }
        } else {
          if (attack == 1) {
            creatures.emplace(attacked.pos, attacked);
          } else {
            assert(attack == 2);
            new_creatures.emplace(attacked.pos, attacked);
          }
        }
      }
    }
    creatures = new_creatures;
    cerr << "end of round " << rounds + 1 << endl;
    for (pair<Coord<>,Creature> creature: creatures) {
      cerr << format_creature(creature.second) << endl;
    }
    cerr << format_map(map, creatures);
    if (continue_combat) {
      rounds++;
    }
  }

  cout << "Full rounds " << rounds << endl;

  int accumulated_hit_points = 0;
  cerr << "HP: ";
  for (pair<Coord<>,Creature> creature : creatures) {
    cerr << creature.second.hit_points << " ";
    accumulated_hit_points += creature.second.hit_points;
  }
  cerr << endl;
  cout << "Hit points " << accumulated_hit_points << endl;
  cout << "Solution: " << accumulated_hit_points * rounds << endl;
  return 0;
}
