#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <list>
#include <sstream>
#include <iomanip>
#include <algorithm>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::list;
using std::vector;
using std::pair;
using std::ostringstream;
using std::string;
using std::setw;
using std::max;

typedef list<int> Ring;
typedef list<int>::const_iterator Iterator;

string to_s(const Ring& ring, const Iterator& pos, int player) {
  ostringstream out;

  out << "[" << player + 1 << "] ";
  for (int marble : ring) {
    if (marble == *pos) {
      out << "(" << setw(2) << marble << ")";
    } else {
      out << " " << setw(2) << marble << " ";
    }
  }
  return out.str();
}

Iterator next(const Ring& ring, Iterator pos) {
  Iterator next = pos;
  next++;

  if (next == ring.end()) {
    return ring.begin();
  } else {
    return next;
  }
}

Iterator prev(const Ring& ring, Iterator pos) {
  if (pos == ring.begin()) {
    pos = ring.end();
  }
  pos--;
  return pos;
}

// insert after the current position, and return an iterator to the inserted
// element
Iterator insert(Ring* ring, Iterator pos, int marble) {
  pos++;
  return ring -> insert(pos, marble);
}

// erase the current position, and return an iterator to the element after
Iterator erase(Ring* ring, Iterator pos) {
  pos = ring -> erase(pos);
  if (pos == ring -> end()) {
    pos = ring-> begin();
  }
  return pos;
}

int main(void) {
  cout << "Players: ";
  int players;
  cin >> players;
  cout << "Marbles: ";
  int marbles;
  cin >> marbles;
  cerr << endl;

  cerr << players << " plyers with " << marbles << " marbles" << endl;
  Ring ring { 0 };
  Iterator pos = ring.begin();
  int player = 0;
  vector<long> scores(players);
  long max_score = 0;

  for (int marble = 1; marble <= marbles; marble++) {
    if (marble % 23 != 0) {
      pos = next(ring, pos);
      pos = insert(&ring, pos, marble);

    } else {
      scores[player] += marble;
      cerr << "player " << player << " scoring " << marble
           << " for a total of " << scores[player] << endl;

      for (int i = 0; i < 7; i++) {
        pos = prev(ring, pos);
      }
      scores[player] += *pos;
      max_score = max(max_score, scores[player]);
      cerr << "player also scoring " << *pos
           << " for a total of " << scores[player] << endl;
      pos = erase(&ring, pos);
    }

    // cerr << to_s(ring, pos, player) << endl;
    player = (player + 1) % players;
  }
  cout << "Solution: " << max_score << endl;
  return 0;
}
