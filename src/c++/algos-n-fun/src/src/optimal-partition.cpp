/* Copyright 2018 <samuelrivas@gmail.com
 *
 * partition jobs in a n optimal way
 */
#include <vector>
#include <iostream>
#include <iomanip>
#include <utility>

using std::vector;
using std::cout;
using std::endl;
using std::setw;
using std::max;

vector<vector<int>> optimal_partition(vector<int> p, int num_partitions) {
  const int N = p.size();
  vector<vector<int>> table(num_partitions, vector<int>(N));
  vector<int> sums(N);

  sums[0] = p[0];
  for (int i = 1; i < N; i++) {
    sums[i] = p[i] + sums[i - 1];
  }

  // Boundary
  for (int j = 0; j < N; j++) {
    table[0][j] = sums[j];
  }

  // Fill in
  for (int i = 1; i < num_partitions; i++) {
    for (int j = 0; j < N; j++) {
      int min_cost = table[i - 1][j];
      for (int k = i; k <= j; k++) {
        int cost = max(table[i - 1][k-1], sums[j] - sums[k - 1]);
        if (min_cost > cost) {
          min_cost = cost;
          // solutions[i] = k;
        }
      }
      table[i][j] = min_cost;
    }
  }

  // for (int i: solutions) {
  //   cout << setw(3) << i;
  // }
  // cout << endl;
  // cout << endl;
  return table;
}

void print_table(vector<vector<int>> table) {

  cout << "   ";

  for (size_t i = 0; i < table[0].size(); i++) {
     cout << setw(4) << i + 1;
  }
  cout << endl;

  int k = table.size();
  for (size_t i = table.size() - 1; i < table.size(); i--) {
    cout << setw(2) << k-- << " ";
    for (int c : table[i]) {
      cout << setw(4) << c;
    }
    cout << endl;
  }
}

int main(void) {
  // print_table(optimal_partition({1, 2, 3, 4, 5, 6, 7}, 3));
  print_table(optimal_partition({1, 2, 3, 4}, 3));

  return 0;
}