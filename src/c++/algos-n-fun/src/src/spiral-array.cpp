#include <iostream>
#include <iomanip>
#include <vector>

using std::cout;
using std::setw;
using std::endl;
using std::vector;
using std::cin;

vector<vector<int>> spiral(int n) {
  vector<int> row(n);
  vector<vector<int>> table(n, row);

  int count = 1;
  int min_x = 0;
  int max_x = n - 1;
  int min_y = 0;
  int max_y = n - 1;

  while (min_x <= max_x) {
    for (int i = min_x; i <= max_x; i++) {
      table[i][min_y] = count++;
    }
    min_y++;

    for (int i = min_y; i <= max_y; i++) {
      table[max_x][i] = count++;
    }
    max_x--;

    for (int i = max_x; i >= min_x; i--) {
      table[i][max_y] = count++;
    }
    max_y--;

    for (int i = max_y; i >= min_y; i--) {
      table[min_x][i] = count++;
    }
    min_x++;
  }
  return table;
}

int main(void) {
  int size;
  cin >> size;

  vector<vector<int>> table = spiral(size);

  for (int i = 0; i < size; i++) {
    for (int j = 0; j < size; j++) {
      cout << setw(3) << table[j][i];
    }
    cout << endl;
  }
  return 0;
}
