// Copyright (C) 2018 by samuelrivas@gmail.com

#include <queue>
#include <unordered_set>
#include <vector>
#include <iostream>

using std::priority_queue;
using std::unordered_set;
using std::vector;
using std::cout;
using std::endl;

struct ReverseOrder {
  bool operator() (int a, int b) {
    return b < a;
  }
};

void push_if_new(int n, priority_queue<int, vector<int>, ReverseOrder>* q,
                 unordered_set<int>* seen) {
  if (seen -> find(n) == seen -> end()) {
    q -> push(n);
    seen -> insert(n);
  }
}

// This writes the sequence for debugging
int main(void) {
  constexpr int N = 100;
  priority_queue<int, vector<int>, ReverseOrder> q;
  unordered_set<int> seen { 3, 5, 7 };

  q.push(3);
  q.push(5);
  q.push(7);

  for (int i = 0; i < N; i++) {
    int next = q.top();
    q.pop();

    push_if_new(next * 3, &q, &seen);
    push_if_new(next * 5, &q, &seen);
    push_if_new(next * 7, &q, &seen);

    cout << next << endl;
  }

  return 0;
}
