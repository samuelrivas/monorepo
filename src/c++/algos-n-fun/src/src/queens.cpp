/* We index the board as a double array where the first dimension is rows, top down, and the second is columns, left to right.

We also index diagonals:

Bottom left to top right diagonals are indexed between 0 and 14 (inclusive) such as cell[x][y] is in diagonal x + y.

Top left to bottom right diagonals are indexed so that cell[x][y] is in diagonal 7 + (x - y) (arbitrarily keeping this in the same index space, though is not really needed) */

#include <unordered_set>
#include <set>
#include <utility>
#include <cassert>
#include <iostream>
#include <vector>
#include <array>

using std::unordered_set;
using std::pair;
using std::cout;
using std::endl;
using std::vector;
using std::set;
using std::array;

typedef array<bool, 8> RowCol;
typedef array<bool, 15> Diag;
typedef pair<int, int> Coord;

void queens(set<Coord> positions, RowCol rows, RowCol cols, Diag bltr, Diag tlbr, set<set<Coord>>* solutions) {
  if (positions.size() == 8) {
    solutions -> insert(positions);
    return;
  }

  for (int row = 0; row < 8; row++) {
    for (int col = 0; col < 8; col++) {
      if (rows[row] && cols[col] && bltr[row + col] && tlbr[7 + row - col]) {
        set<Coord> new_positions(positions);
        new_positions.insert(pair<int, int>(row, col));

        RowCol new_rows(rows);
        new_rows[row] = false;
        RowCol new_cols(cols);
        new_cols[col] = false;

        Diag new_bltr(bltr);
        new_bltr[row + col] = false;

        Diag new_tlbr(tlbr);
        new_tlbr[7 + row - col] = false;

        queens(move(new_positions), move(new_rows), move(new_cols), move(new_bltr), move(new_tlbr), solutions);
      }
    }
  }
}

set<set<Coord>> queens() {
  set<set<Coord>> solutions;
  set<Coord> positions;
  RowCol rows;
  rows.fill(true);
  RowCol cols;
  cols.fill(true);
  Diag bltr;
  bltr.fill(true);
  Diag tlbr;
  tlbr.fill(true);

  queens(move(positions), move(rows), move(cols), move(bltr), move(tlbr), &solutions);
  return solutions;
}

int main(void) {

  for (set<Coord> solution: queens()) {
    for (Coord coord : solution) {
      cout << "(" << coord.first << "," << coord.second << ") ";
    }
    cout << endl;
  }
}
