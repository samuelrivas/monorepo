#include <vector>
#include <iostream>
#include <sstream>
#include <utility>
#include <string>
#include <cassert>
#include <unordered_set>

using std::vector;
using std::istringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::getline;
using std::pair;
using std::string;
using std::unordered_set;

typedef struct Claim {
  int id;
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

  // -1 if unclaimed
  vector<vector<int>> fabric(max_x, vector<int>(max_y, -1));

  // Some speed, as usual
  // cin.sync_with_stdio(false);

  unordered_set<int> good_claims;

  for (string line; getline(cin, line) && line.size() > 0;) {
    Claim claim = parse_claim(line);

    good_claims.insert(claim.id);

    for (size_t x = claim.x; x < claim.x + claim.width; x++) {
      for (size_t y = claim.y; y < claim.y + claim.height; y++) {
        assert(x < max_x);
        assert(y < max_y);

        if (fabric[x][y] != -1) {
          cerr << fabric[x][y] << " bombs with " << claim.id << endl;
          good_claims.erase(fabric[x][y]);
          good_claims.erase(claim.id);
        }
        fabric[x][y] = claim.id;
      }
    }
    cerr << "# Valid claims so far: " << good_claims.size() << endl;
  }

  cout << "Valid claims: ";
  for (int id : good_claims) {
    cout << id << " ";
  }
  cout << endl;
  return 0;
}
