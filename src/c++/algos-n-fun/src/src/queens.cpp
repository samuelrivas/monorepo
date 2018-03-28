/* We index the board as a double array where the first dimension is rows, top
   down, and the second is columns, left to right.

   We also index diagonals:

   Bottom left to top right diagonals are indexed between 0 and 14 (inclusive)
   such as cell[x][y] is in diagonal x + y.

   Top left to bottom right diagonals are indexed so that cell[x][y] is in
   diagonal 7 + (x - y) (so that we can directly map this to an array index)
 */
#include <unordered_set>
#include <utility>
#include <iostream>
#include <vector>
#include <array>

using std::unordered_set;
using std::pair;
using std::cout;
using std::endl;
using std::vector;
using std::array;

typedef array<bool, 15> Diag;
typedef pair<int, int> Coord;

void queens(array<Coord, 8> partial, int col, unordered_set<int> avail_rows,
            Diag avail_bltr, Diag avail_tlbr,
            vector<array<Coord, 8>>* solutions) {

  if (col == 8) {
    solutions -> push_back(move(partial));
    return;
  }

  for (int row : avail_rows) {
    if (avail_bltr[row + col] && avail_tlbr[7 + row - col]) {
      array<Coord, 8> new_partial = partial;
      new_partial[col] = pair<int, int>(row, col);
      unordered_set<int> new_avail_rows(avail_rows);
      new_avail_rows.erase(row);
      Diag new_avail_bltr(avail_bltr);
      new_avail_bltr[row + col] = false;
      Diag new_avail_tlbr(avail_tlbr);
      new_avail_tlbr[7 + row - col] = false;

      queens(move(new_partial), col + 1, move(new_avail_rows),
             move(new_avail_bltr), move(new_avail_tlbr), solutions);
    }
  }
}

vector<array<Coord, 8>> queens() {
  vector<array<Coord, 8>> solutions;
  array<Coord, 8> partial;
  unordered_set<int> rows {0, 1, 2, 3, 4, 5, 6, 7};
  Diag bltr;
  bltr.fill(true);
  Diag tlbr;
  tlbr.fill(true);

  queens(move(partial), 0, move(rows), move(bltr), move(tlbr), &solutions);
  return solutions;
}

int main(void) {

  for (array<Coord, 8> solution: queens()) {
    for (Coord coord : solution) {
      cout << "(" << coord.first << "," << coord.second << ") ";
    }
    cout << endl;
  }
}
