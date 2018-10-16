#include <iostream>
#include <cassert>
#include <vector>
#include <utility>
#include <set>
#include <iomanip>

using std::cout;
using std::endl;
using std::vector;
using std::pair;
using std::set;
using std::setw;

set<set<int>>
combinations(const set<int>& friends, int k) {
  assert(k > 0);
  assert(k <= static_cast<int>(friends.size()));

  if (friends.size() == static_cast<size_t>(k)) {
    return { friends };
  }

  if (k == 1) {
    set<set<int>> sittings;
    for (int f : friends) {
      sittings.insert({ f });
    }
    return sittings;
  }

  // Copy friends, but remove one (we call him loner)
  set<int> other_friends { friends };
  int lonely_friend = *friends.begin();
  other_friends.erase(lonely_friend);

  // Get sittings for all the other friends
  set<set<int>> sittings = combinations(other_friends, k);

  // Now get sittings with loner and add them to those
  set<set<int>> sittings_with_room_for_loner = combinations(other_friends, k - 1);
  for (set<int> sitting : sittings_with_room_for_loner) {
    sitting.insert(lonely_friend);
    sittings.insert(sitting);
  }
  return sittings;
}

int main(void) {
  vector<pair<set<int>, int>> tests {
    {{ 1 }, 1},
    {{ 1, 2, 3, 4, 5, 6 }, 3},
    {{ 1, 2, 3, 4, 5 }, 5},
    {{ 1, 2, 3, 4, 5 }, 4},
    {{ 1, 2, 3, 4, 5 }, 3},
    {{ 1, 2, 3, 4, 5 }, 2}
  };

  for (auto test : tests) {
    cout << "**** (" << test.first.size() << "," << test.second << "): " << endl;
    for (auto sitting :  combinations(test.first, test.second)) {
      for (int person : sitting) {
        cout << setw(2) << person;
      }
      cout << endl;
    }
  }
}
