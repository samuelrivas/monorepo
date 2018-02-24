/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Approximate string matching with configurable costs
 */
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>

using std::string;
using std::vector;
using std::min;
using std::cout;
using std::endl;
using std::setw;

enum class Op {
  Match,
  Insert,
  Delete
};

int cost_insert(__attribute__((unused)) char a) {
  return 1;
}

int cost_skip(__attribute__((unused)) char a) {
  return 5;
}

int cost_match(char pattern, char text) {
  return pattern == text ? 0 : 10;
}

vector<vector<int>> approximate_match_table(string pattern, string text) {
  const int M = pattern.size() + 1;
  const int N = pattern.size() + 1;

  vector<vector<int>> table(M, vector<int>(N, 0));

  // Boundaries
  for (int j = 1; j < N; j ++) {
    table[0][j] = table[0][j - 1] + cost_insert(text[j - 1]);
  }

  for (int i = 1; i < M; i++) {
    table[i][0] = table [i - 1][0] + cost_skip(pattern[i - 1]);
  }

  // Fill in table
  for (int i = 1; i < M; i++) {
    for (int j = 1; j < N; j++) {
      int match = cost_match(pattern[i-1], text[j-1]) + table[i-1][j-1];
      int skip = cost_skip(pattern[i-1]) +  table[i - 1][j];
      int insert = cost_insert(text[j - 1]) + table[i][j - 1];

      int cost = min(min(match, skip), insert);

      table[i][j] = cost;
    }
  }
  return table;
}

void print_table(vector<vector<int>> table, string pattern, string text) {

  cout << "      ";

  for (char c : text) {
    cout << "  " << c;
  }

  cout << endl;

  for (size_t i = 0; i < pattern.size() + 1; i++) {
    if (i == 0) {
      cout << "   ";
    } else {
        cout << " " << pattern[i - 1] << " ";
    }
    for (size_t j = 0; j < text.size() + 1; j++) {
      cout << setw(3) << table[i][j];
    }
    cout << endl;
  }
}

int main(void) {

  string text { "Samuel Rivas" };
  string pattern { "Mr Sam Ribas" };

  print_table(approximate_match_table(pattern, text), pattern, text);

  return 0;
}
