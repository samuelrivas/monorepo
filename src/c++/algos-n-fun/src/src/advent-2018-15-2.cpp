/* This aborts if one elf dies. And then I just run a sloppy binary search on
   the input to find the minimum power necessary to save all elves */
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
#include <limits>

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
using std::priority_queue;
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
  int power;
  char type;

  Creature(char _type, int _power, Coord<> _pos) :
    pos { _pos },
    power { _power },
    type { _type } { }

  // Hit this creature
  void hit(int hit_power) {
    hit_points -= hit_power;
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

struct BfsNode {
  Coord<> parent;
  int distance;
  Coord<> pos;
};

struct BfsPriority {
  // Confusingly, this returns whether a has less priority than b (i.e. it comes
  // after out of the queue)
  bool operator()(const BfsNode& a, const BfsNode&b) const {
    if (a.distance != b.distance) {
      return a.distance > b.distance;
    }

    CoordComp comp;
    if (a.pos == b.pos) {
      return comp(b.parent, a.parent);
    }
    return comp(b.pos, a.pos);
  }
};

pair<Map,Creatures> parse_map(const vector<string>& input, int elf_power) {
  Map map;
  Creatures creatures;

  for (int y = 0; y < static_cast<int>(input.size()); y++) {
    vector<char> map_line;

    for (int x = 0; x < static_cast<int>(input[y].size()); x++) {
      char c = input[y][x];
      if (c == 'G' || c == 'E') {
        int power = (c == 'E') ? elf_power : 3;
        creatures.emplace(Coord<>({ x, y }), Creature(c, power, { x, y }));
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
  priority_queue<BfsNode, vector<BfsNode>, BfsPriority> q;

  q.push({{-1, -1}, 0, creature.pos});
  bool found = false;
  CoordMap parents;
  BfsNode expanding;

  // cerr << "Starting search in " << format_coord(creature.pos)
  //      << " for a " << creature.type << endl;
  while (!found && q.size() > 0) {
    expanding = q.top();
    q.pop();

    // cerr << "Expanding " << format_coord(expanding.pos)
    //      << " from " << format_coord(expanding.parent)
    //      << " distance " << expanding.distance
    //      << endl;
    if (parents.find(expanding.pos) != parents.end()) {
      // cerr << "Already expanded" << endl;
      continue;
    }

    parents[expanding.pos] = expanding.parent;

    for (Coord<> delta : deltas) {
      BfsNode to_expand = { expanding.pos,
                            expanding.distance + 1,
                            sum_coord(expanding.pos, delta)
      };
      if (read_map(map, to_expand.pos) == '.'
          && !has_creature(creatures, to_expand.pos, creature.type)
          && !has_creature(new_creatures, to_expand.pos, creature.type)) {
        // cerr << "Adding to queue " << format_coord(to_expand.pos)
        //      << " distance " << to_expand.distance
        //      << " parent " << format_coord(to_expand.parent) << endl;
        q.push(to_expand);
      }

      if (has_creature(creatures, to_expand.pos, opposite(creature.type))
          || has_creature(new_creatures, to_expand.pos, opposite(creature.type))) {
        // cerr << "Found a valid target in " << format_coord(to_expand.pos)
        //      << " while expanding " << format_coord(expanding.pos) << endl;
        found = true;
        break;
      }
    }
  }

  if (found) {
    Coord<> parent = expanding.pos;
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


int main(int argc, char *argv[]) {

  int elf_power;
  if (argc == 2) {
    istringstream input(argv[1]);
    input >> elf_power;
  } else {
    elf_power = 3;
  }

  cerr << "Elf Power: " << elf_power <<  endl;

  cin.sync_with_stdio(false);

  vector<string> input_lines;
  for (string line; getline(cin, line);) {
    input_lines.push_back(line);
  }
  Map map;
  Creatures creatures;
  {
    pair<Map,Creatures> parsed = parse_map(input_lines, elf_power);
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
        Creature attacked('x', 0, {-1, -1});
        if (attack == 1) {
          attacked = creatures.at(attack_coord);
          creatures.erase(attack_coord);
        } else if (attack == 2) {
          attacked = new_creatures.at(attack_coord);
          new_creatures.erase(attack_coord);
        }

        attacked.hit(creature.power);
        cerr << "Attacked " << attacked.type
             << "(" << attacked.hit_points << ") in "
             << format_coord(attacked.pos) << endl;

        if (!attacked.alive()) {
          cerr << "And is dead!" << endl;
          if (attacked.type == 'E') {
            elves --;
            abort();
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
