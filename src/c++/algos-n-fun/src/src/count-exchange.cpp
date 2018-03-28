// Copyright (C) 2018 by samuelrivas@gmail.com
#include <unordered_set>
#include <set>
#include <utility>
#include <cassert>
#include <iostream>
#include <vector>

using std::unordered_set;
using std::pair;
using std::cout;
using std::endl;
using std::vector;
using std::set;

void print_solution(const set<pair<int, int>>& solution) {
  for (pair<int, int> p : solution) {
    cout << "(" << p.first << "," << p.second << ") ";
  }
  cout << endl;
}

// The partial set is just for being able to debug this. It is not needed to
// count the amount of solutions
int count_exchange(const int& n, const unordered_set<int>& coins,
                   set<pair<int, int>> partial) {
  assert(n >= 0);

  if (n == 0) {
    print_solution(partial);
    return 1;
  }

  int count = 0;
  unordered_set<int> new_coins(coins);

  for (int coin : coins) {
    new_coins.erase(coin);
    for (int paid = coin; paid <= n; paid += coin) {
      set<pair<int, int>> new_partial(partial);
      new_partial.insert(pair<int, int>(paid, coin));
      count += count_exchange(n - paid, new_coins, new_partial);
    }
  }
  return count;
}

int count_exchange(const int& n, const unordered_set<int>& coins) {
  set<pair<int, int>> partial;
  return count_exchange(n, coins, move(partial));
}

int main(void) {
  unordered_set<int> coins {25, 10, 5, 1};
  vector<int> tests {7, 16, 30, 0, 3};

  for (int test : tests) {
    cout << test << ":" << endl;
    int solutions = count_exchange(test, coins);
    cout << "that is: " << solutions << endl;
  }
  return 0;
}
