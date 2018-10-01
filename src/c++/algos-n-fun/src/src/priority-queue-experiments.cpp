#include <iostream>
#include <queue>
#include <vector>
#include <iomanip>
#include <functional>

using std::cout;
using std::vector;
using std::endl;
using std::priority_queue;
using std::setw;

class Compare {
public:
  bool operator()(int a, int b) {
    return a > b;
  }
} compare;

int main(void) {

  vector<int> test { 3, 2, 5, 2, 6, 7, 1 };
  priority_queue<int, vector<int>, std::greater<int>> queue;

  for (int i : test) {
    queue.push(i);
  }

  while (! queue.empty()) {
    cout << setw(2) << queue.top();
    queue.pop();
  }

  cout << endl;

  return 0;
}
