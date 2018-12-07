#include <iostream>
#include <string>
#include <map>
#include <utility>

#include "lib/rabin-karp.hpp"

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::getline;
using std::map; // This should be unordered_map for efficiency, but I don't care
                // enough to get the pairs to hash right now
using std::pair;

int main() {

  //  Map pairs <position, hash> to the string that generated them
  map<pair<int, int>, string> hashes;

  cin.sync_with_stdio(false);

  for (string line; getline(cin, line);) {

    // Generate a hash for each removed letter and position
    for (size_t i = 0; i < line.size(); i++) {

      // Use a large size, as we don't want to do a rolling hash, just using our
      // RK for convenience
      RKHasher<'z' - 'a' + 1> hasher(100);
      for (size_t j = 0; j < line.size(); j++) {
        if (j != i) {
          hasher.push(line[j]);
        }
      }

      int hash = hasher.get_hash();
      if (hashes.find({i, hash}) != hashes.end()) {
        // Bravely ignoring collisions here, and it worked for the task we
        // needed to solve
        string match = hashes[{i, hash}];
        cout << "Found:" << endl
             << match << endl
             << line << endl
             << "removing " << i << "(" << line[i] << ")" << endl
             << "That is : ";

        for (size_t pos = 0; pos < line.size(); pos++) {
          if (pos != i) {
            cout << line[pos];
          }
        }
        cout << endl;
        return 0;
      } else {
        hashes[{i, hash}] = line;
      }
    }
  }

  cout << "Not found!" << endl;
  return 0;
}
