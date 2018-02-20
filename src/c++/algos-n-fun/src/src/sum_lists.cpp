/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Sum lists representing reversed numbers in decimal notation
 */
#include <forward_list>
#include <iostream>
#include <vector>
#include <utility>

using std::forward_list;
using std::cout;
using std::endl;
using std::vector;
using std::pair;

void sum_rec(forward_list<int>* a,
             forward_list<int>* b,
             int carry,
             forward_list<int>* result);

forward_list<int> sum(forward_list<int> a, forward_list<int> b) {
  forward_list<int> result { };

  sum_rec(&a, &b, 0, &result);
  return result;
}

void sum_rec(forward_list<int>* a,
             forward_list<int>* b,
             int carry,
             forward_list<int>* result) {
  if (a -> empty() && b -> empty()) {
    if (carry > 0) {
      result -> push_front(carry);
    }
    return;
  }

  int a_digit = (a -> empty()) ? 0 : a -> front();
  int b_digit = (b -> empty()) ? 0 : b -> front();
  int partial_sum = a_digit + b_digit + carry;

  if (!a -> empty()) {
    a -> pop_front();
  }

  if (!b -> empty()) {
    b -> pop_front();
  }

  sum_rec(a, b, partial_sum / 10, result);
  result -> push_front(partial_sum % 10);
}

int main(void) {
  vector<pair<forward_list<int>, forward_list<int>>> tests {
    { { }, { } },
    { { 1 }, { } },
    { { }, { 1 } },
    { { 1, 2 }, { 1, 2 } },
    { { 1 }, { 9, 9 } },
    { { 9 }, { 9, 9, 9, 9, 9 } },
    { { 3, 1 }, { 0, 0, 0, 1 } }
  };

  for (auto test : tests) {
    for (int n : test.first) {
      cout << n;
    }

    cout << " + ";

    for (int n : test.second) {
      cout << n;
    }

    forward_list<int> result = sum(test.first, test.second);

    cout << " = ";

    for (int n : result) {
      cout << n;
    }
    cout << endl;
  }
}
