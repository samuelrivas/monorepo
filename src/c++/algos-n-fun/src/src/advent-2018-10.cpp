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

typedef struct BBox {
  Coord min = { numeric_limits<int>::max(), numeric_limits<int>::max() };
  Coord max = { numeric_limits<int>::min(), numeric_limits<int>::min() };
} BBox;

typedef struct Particle {
  Coord pos;
  Coord v;
} Particle;

string format_coord(const Coord& c) {
  ostringstream out;
  out << "(" << c.x << "," << c.y << ")";
  return out.str();
}

Coord sum_coord(const Coord& a, const Coord& b) {
  return { a.x + b.x, a.y + b.y };
}

string format_particle(const Particle& p) {
  ostringstream out;
  out << format_coord(p.pos) << " -- " << format_coord(p.v);
  return out.str();
}

Particle parse_line(const string& line) {
  regex line_regex("position=< *(-?[[:digit:]]+), *(-?[[:digit:]]+)> velocity=< *(-?[[:digit:]]+), *(-?[[:digit:]]+)>");
  smatch matches;

  if (!regex_match(line, matches, line_regex)) {
    cerr << "This line kinda doesn't match:" << line << endl;
    terminate();
  }
  Particle p;
  istringstream(matches[1]) >> p.pos.x;
  istringstream(matches[2]) >> p.pos.y;
  istringstream(matches[3]) >> p.v.x;
  istringstream(matches[4]) >> p.v.y;
  return p;
}

BBox update_bbox(const BBox& old_bbox, const Coord& coord) {
  BBox bbox;

  bbox.min.x = min(coord.x, old_bbox.min.x);
  bbox.min.y = min(coord.y, old_bbox.min.y);
  bbox.max.x = max(coord.x, old_bbox.max.x);
  bbox.max.y = max(coord.y, old_bbox.max.y);

  return bbox;
}

pair<BBox, vector<Particle>> parse_input(void) {
  vector<Particle> particles;

  BBox bbox;

  for (string line; getline(cin, line);) {
    Particle p = parse_line(line);
    bbox = update_bbox(bbox, p.pos);
    particles.push_back(p);
  }
  return { bbox, particles };
}

pair<BBox,vector<Particle>> next_particles(const vector<Particle>& particles) {
  vector<Particle> output;
  BBox bbox;

  for (Particle p : particles) {
    p.pos = sum_coord(p.pos, p.v);
    bbox = update_bbox(bbox, p.pos);
    output.push_back(p);
  }
  return { bbox, output };
}

string render_particles(const vector<Particle>& particles, const BBox& bbox) {
  int width = bbox.max.x - bbox.min.x + 1;
  int height = bbox.max.y - bbox.min.y + 1;

  vector<vector<bool>> table(width, vector<bool>(height));

  for (Particle p : particles) {
    table[p.pos.x - bbox.min.x][p.pos.y - bbox.min.y] = true;
  }

  ostringstream out;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      out << (table[x][y] ? '#' : ' ');
    }
    out << endl;
  }
  return out.str();
}

int main(void) {
  cin.sync_with_stdio(false);
  pair<BBox, vector<Particle>> input = parse_input();

  BBox past_bbox = input.first;
  vector<Particle> past_particles = input.second;

  cerr << "Initial BBox: " << format_coord(past_bbox.min)
       << " -- " << format_coord(past_bbox.max) << endl;

  int seconds = 0;
  while (true) {
    pair<BBox,vector<Particle>> next = next_particles(past_particles);
    BBox bbox = next.first;

    // cerr << "BBox: " << format_coord(bbox.min)
    //      << " -- " << format_coord(bbox.max) << endl;

    if (bbox.min.x < past_bbox.min.x
        || bbox.min.y < past_bbox.min.y
        || bbox.max.x > past_bbox.max.x
        || bbox.max.y > past_bbox.max.y) {
      break;
    }
    seconds++;
    past_particles = next.second;
    past_bbox = bbox;
  }
  cout << render_particles(past_particles, past_bbox) << endl;
  cout << "After: " << seconds << " seconds" << endl;
  return 0;
}
