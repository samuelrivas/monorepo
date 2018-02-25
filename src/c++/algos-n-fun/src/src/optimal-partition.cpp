/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Partition jobs in a n optimal way. Not pretty, but not terrible either
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
using std::pair;

// For debugging
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

/* Given a vector of integers p and how many partitions (as much) we can choose,
   find the optimal way of dividing the vector in subvectors so that the maximum
   weight (sum of the elements in the subvector) is minimal.

   The solution is a vector with the number of elements in each partition. That
   is solution[0] is the number of elements in the left-most partition. */
vector<int> optimal_partition(vector<int> p, int k) {
  const int N = p.size();

  /* table[i][j] is the weight of the best solution for i + 1 elements and j + 1
     partitions
   */
  vector<vector<int>> table(k, vector<int>(N));

  /* partitions[j][j] is the number of elements in the last partition, for the
     corresponding solution in table[i][j] */
  vector<vector<int>> partitions(k, vector<int>(N));

  /* Cache to avoid computing the sum of a partition repeated times */
  vector<int> sums(N);
  sums[0] = p[0];
  for (int i = 1; i < N; i++) {
    sums[i] = p[i] + sums[i - 1];
  }

  /* Boundary conditions. Using just one partition, the weight is the same as
     the sum of all elements */
  for (int j = 0; j < N; j++) {
    table[0][j] = sums[j];
    partitions[0][j] = j + 1;
  }

  /* Fill in the dynamic table. The recurrent step is somewhat hard to explain
     in text, but the idea is to get the best solution piciking the solution for
     m elements and one less partition and adding the remaining n - m as a new
     partition */
  for (int i = 1; i < k; i++) {
    for (int j = 0; j < N; j++) {
      table[i][j] = table[i - 1][j];
      partitions[i][j] = 0;

      for (int partition = 1; partition <= j; partition++) {
        int cost = max(table[i - 1][j - partition],
                       sums[j] - sums[j - partition]);

        if (table[i][j] > cost) {
          table[i][j] = cost;
          partitions[i][j] = partition;
        }
      }
    }
  }

  /* The latest partition size X is the optimum size for the kth partition. That
     is based on the solution for k - 1 partitions and n - X elements, and so on
     ... */
  vector<int> solution(k);
  int pos = N - 1;
  for (size_t i = solution.size() - 1; i < solution.size(); i --) {
    solution[i] = partitions[i][pos];
    pos-= solution[i];
  }

  // debug, print the internal tables
  //
  // print_table(partitions);
  // cout << endl;
  // print_table(table);
  // cout << "solution vector :" << endl;
  // for (int i : solution) {
  //   cout << setw(3) << i;
  // }
  // cout << endl;

  return solution;
}

void print_solution(vector<int> solution, vector<int> p) {

  cout <<  "Elements  :";
  for (int element : p) {
    cout << " " << element;
  }
  cout << endl << "Partitions: " << solution.size()  << endl;

  int pos = 0;

  for (auto sol : solution) {
    int accum = 0;
    for (int i = 0; i < sol; i++) {
      cout << setw(3) << p[pos];
      accum += p[pos++];
    }
    cout << " - " << accum << endl;
  }
}

int main(void) {
  vector<pair<vector<int>, int>> tests {
    {{1, 2, 3, 4}, 2},
    {{1, 2, 3, 4}, 3},
    {{1, 2, 3, 4, 5, 6, 7} , 2},
    {{1, 2, 3, 4, 5, 6, 7} , 4},
    {{1, 2, 3, 4, 5, 6, 7} , 5},
    {{1, 2, 3, 4, 5, 6, 7} , 6},
    {{1, 1, 1, 1, 1, 1, 1, 1, 1}, 3},
    {{1, 2, 3, 4, 5, 6, 7, 8, 9}, 3}
  };

  for (auto test : tests) {
    auto solution = optimal_partition(test.first, test.second);
    print_solution(solution, test.first);
    cout << endl;
  }
  return 0;
}
