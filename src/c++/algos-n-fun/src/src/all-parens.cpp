#include <unordered_set>
#include <string>
#include <cassert>
#include <iostream>

using std::unordered_set;
using std::string;
using std::cout;
using std::endl;

void all_parens(int l, int r, string str, unordered_set<string>* solutions) {
  if (l < 0 || r < l) {
    return;
  }
  if (l == 0 && r == 0) {
    solutions -> insert(str);
    return;
  }
  if (l > 0) {
    all_parens(l - 1, r, str + "(", solutions);
  }
  if (r > l) {
    all_parens(l, r - 1, str + ")", solutions);
  }
}

unordered_set<string> all_parens(int n) {
  unordered_set<string> solutions;
  all_parens(n, n, "", &solutions);
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
