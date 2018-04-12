#include <vector>
#include <utility>
#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <iomanip>

using std::vector;
using std::pair;
using std::string;
using std::min;
using std::endl;
using std::cout;
using std::ostringstream;
using std::setw;

bool match_aux(char x, char y) {
  return (x == 'c' && y == 'g')
    || (x == 'a' && y == 'u');
}

bool match(char x, char y) {
  return match_aux(x, y)
    || match_aux(y, x);
}

typedef vector<vector<int>> Table;

int best_fold_cost(const string& sequence, int x, int y, int freedom, const Table& table) {
  int min_cost = table[x][y - 1] + 1;
  for (int i = x; i < y - freedom; i++) {
    if (match(sequence[i], sequence[y])) {
      int cost;
      if (i == x) {
        cost = table[i + 1][y - 1];
      } else {
        cost = table[x][i - 1] + table[i + 1][y - 1];
      }
      min_cost = min(min_cost, cost);
    }
  }
  return min_cost;
}

// Freedom is the amount of nucleotides needed between separate bonds
Table strand_costs(const string& sequence, size_t freedom) {
  Table table(sequence.size(), vector<int>(sequence.size()));

  for (size_t y = 0; y < sequence.size(); y++) {
    for (size_t x = 0; x < sequence.size(); x++) {
      if (x > y) {
        table[x][y] = -9; // for debugging
      } else if (y - x <= freedom) {
        table[x][y] = y - x + 1;
      } else {
        table[x][y] = best_fold_cost(sequence, x, y, freedom, table);
      }
    }
  }
  return table;
}

string print_table(const Table& table, const string& sequence) {
  ostringstream out;

  out << endl;
  for (size_t y = table.size() - 1; y < table.size(); y--) {
    out << sequence[y];
    for (size_t x = 0; x < table.size(); x++) {
      out << setw(3) << table[x][y];
    }
    out << endl;
  }

  out << " ";
  for (size_t x = 0; x < table.size(); x++) {
    out << setw(3) << sequence[x];
  }

  return out.str();
}

int main(void) {
  vector<string> tests {
    "uacccgggggggu",
    "uacccggg",
    "accgguagu",
    "acaugauggccaugu",
    "cagaucggcgauacgagcauagcaaugcuaagcgagcuuagcugca"
  };

  for (string test : tests) {
    string table = print_table(strand_costs(test, 4), test);
    cout << table;
    cout << endl;
  }
  return 0;
}
