/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Approximate string matching with configurable costs
 */
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <utility>
#include <cassert>
#include <cstdlib>

using std::string;
using std::vector;
using std::min;
using std::cout;
using std::endl;
using std::setw;
using std::pair;

enum class Op {
  Match,
  Insert,
  Skip
};

string to_string(Op op) {
  switch(op) {
  case Op::Match:
    return "M";
  case Op::Insert:
    return "I";
  case Op::Skip:
    return "S";
  }
  assert(false);
}

int cost_insert(__attribute__((unused)) char a) {
  return 1;
}

int cost_skip(__attribute__((unused)) char a) {
  return 50;
}

int cost_match(char pattern, char text) {
  return pattern == text ? 0 : 10;
}

vector<vector<pair<Op, int>>> approximate_match_table(
                                                      string pattern,
                                                      string text) {
  const int M = pattern.size() + 1;
  const int N = text.size() + 1;

  vector<vector<pair<Op, int>>> table(M,
                                      vector<pair<Op, int>>(N,
                                                            { Op::Match, 0 }));

  // Boundaries
  for (int j = 1; j < N; j ++) {
    table[0][j] = { Op::Insert,
                    table[0][j - 1].second + cost_insert(text[j - 1])
    };
  }

  for (int i = 1; i < M; i++) {
    table[i][0] = { Op::Skip,
                    table [i - 1][0].second + cost_skip(pattern[i - 1])
    };
  }

  // Fill in table
  for (int i = 1; i < M; i++) {
    for (int j = 1; j < N; j++) {
      int cost = cost_match(pattern[i-1], text[j-1]) + table[i-1][j-1].second;
      Op op = Op::Match;

      int skip = cost_skip(pattern[i-1]) +  table[i - 1][j].second;
      if (skip < cost) {
        cost = skip;
        op = Op::Skip;
      }

      int insert = cost_insert(text[j - 1]) + table[i][j - 1].second;
      if (insert < cost) {
        cost = insert;
        op = Op::Insert;
      }

      table[i][j] = {op, cost};
    }
  }
  return table;
}

void print_table_costs(vector<vector<pair<Op, int>>> table, string pattern,
                       string text) {

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
      cout << setw(3) << table[i][j].second;
    }
    cout << endl;
  }
}

void print_table_ops(vector<vector<pair<Op, int>>> table, string pattern,
                     string text) {

  cout << "     ";

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
      cout << " " << to_string(table[i][j].first) << " ";
    }
    cout << endl;
  }
}

void print_solution_rec(vector<vector<pair<Op, int>>> table, string pattern,
                        string text, int i, int j) {
  if (i == 0 && j == 0) {
    assert(table[i][j].first == Op::Match);
    return;
  }
  switch (table[i][j].first) {
  case Op::Match:
    print_solution_rec(table, pattern, text, i - 1, j - 1);
    cout << pattern[i-1] << " " << text[j - 1]
         << " M " << table[i][j].second
         << endl;
    break;
  case Op::Skip:
    print_solution_rec(table, pattern, text, i - 1, j);
    cout << pattern[i - 1] << "  "
         << " S " << table[i][j].second
         << endl;
    break;
  case Op::Insert:
    print_solution_rec(table, pattern, text, i, j - 1);
    cout << "  " << text[j - 1]
         << " I " << table[i][j].second
         << endl;
    break;
  }
}

void print_solution(vector<vector<pair<Op, int>>> table, string pattern,
                    string text) {
  print_solution_rec(table, pattern, text, pattern.size(),
                     text.size());
}

int main(void) {

  string text { "There is a pattern in here" };
  string pattern { "Pattern" };

  auto t = approximate_match_table(pattern, text);
  print_table_costs(t, pattern, text);
  cout << endl;
  print_table_ops(t, pattern, text);
  cout << endl;
  print_solution(t, pattern, text);

  return 0;
}
