#include <vector>
#include <iostream>
#include <sstream>
#include <utility>
#include <string>
#include <cassert>

using std::vector;
using std::istringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::getline;
using std::pair;
using std::string;

typedef struct Claim {
  string id;
  size_t x;
  size_t y;
  size_t width;
  size_t height;
} Claim;

Claim parse_claim(const string& line) {
  istringstream is(line);
  Claim claim;

  // Extremely lazy parsing
  is.ignore(1);
  is >> claim.id;
  is.ignore(3);
  is >> claim.x;
  is.ignore(1);
  is >> claim.y;
  is.ignore(2);
  is >> claim.width;
  is.ignore(1);
  is >> claim.height;

  cerr << claim.id << "|"
       << claim.x << "|"
       << claim.y << "|"
       << claim.width << "|"
       << claim.height << endl;
  return claim;
}

int main(void) {
  const int max_x = 1000;
  const int max_y = 1000;

  // true if claimed
  vector<vector<bool>> fabric(max_x, vector<bool>(max_y, false));

  // true if at least double claimed
  vector<vector<bool>> conflicts(max_x, vector<bool>(max_y, false));

  // Some speed, as usual
  cin.sync_with_stdio(false);

  int conflicting_inches = 0;
  for (string line; getline(cin, line);) {
    Claim claim = parse_claim(line);

    for (size_t x = claim.x; x < claim.x + claim.width; x++) {
      for (size_t y = claim.y; y < claim.y + claim.height; y++) {
        assert(x < max_x);
        assert(y < max_y);

        if (fabric[x][y] && !conflicts[x][y]) {
          conflicts[x][y] = true;
          conflicting_inches++;
          cerr << "counting new conflict at (" << x << "," << y << ")" << endl;
        }

        fabric[x][y] = true;
      }
    }
  }

  // For debugging
  for (size_t y = 0; y < max_y; y++) {
    for (size_t x = 0; x < max_x; x++) {
      // conflict => fabric
      assert(!conflicts[x][y] || fabric[x][y]);
      if (conflicts[x][y]) {
        cerr << "X ";
      } else if (fabric[x][y]) {
        cerr << "# ";
      } else {
        cerr << ". ";
      }
    }
    cerr << endl;
  }

  cout << "Conflicting inches: " << conflicting_inches << endl;
  return 0;
}
