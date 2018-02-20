/* Copyright 2018 <samuelrivas@gmail.com
 *
 * A stakc that also keeps track of the minimum element
 */
#include <stack>
#include <iostream>
#include <vector>
#include <iomanip>

using std::stack;
using std::vector;
using std::cout;
using std::endl;
using std::setw;

class StackWithMin {
 private:
    stack<int> values;
    stack<int> mins;

 public:
  StackWithMin() : values { }, mins { } { };

  void push(int x) {
    values.push(x);
    if (mins.empty() || x < mins.top()) {
      mins.push(x);
    }
  }

  int pop() {
    int value = values.top();
    values.pop();
    if (values.empty() || (value == mins.top() && values.top() > value)) {
      mins.pop();
    }
    return value;
  }

  int min() {
    return mins.top();
  }

  bool empty() {
    return values.empty();
  }
};

int main(void) {
  vector<vector<int>> tests = {
    { 3, 5, 7, 1, 1, 2, 4 },
    { 3, 2, 1, 1, 2, 3 },
    { }
  };

  for (auto test : tests) {
    StackWithMin s;

    cout << "Test: ";
    for (auto i : test) {
      cout << setw(2) << i;
      s.push(i);
    }
    cout << endl;

    while (!s.empty()) {
      int min = s.min();
      cout << "(" << min << "," << s.pop() << ") ";
    }
    cout << endl;
  }
}
