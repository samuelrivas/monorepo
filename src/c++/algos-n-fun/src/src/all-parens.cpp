#include <unordered_set>
#include <string>
#include <cassert>
#include <iostream>

using std::unordered_set;
using std::string;
using std::cout;
using std::endl;

unordered_set<string> all_parens(int n) {
  assert(n >= 0);

  unordered_set<string> solutions;

  if (n <= 1) {
     if (n == 1) {
       solutions.insert("()");
     }
    return solutions;
  }

  unordered_set<string> partials = all_parens(n - 1);

  for (string partial : partials) {
    solutions.insert("()" + partial);
    solutions.insert("(" + partial + ")");
    solutions.insert(partial + "()");
  }
  return solutions;
}

int main(void) {
  string separator = "";

  unordered_set<string> solutions = all_parens(3);

  for (string solution : solutions) {
    cout << separator << solution;
    separator = ", ";
  }
  cout << endl;
  return 0;
}
