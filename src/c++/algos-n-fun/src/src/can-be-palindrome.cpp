#include <iostream>
#include <unordered_map>
#include <string>
#include <vector>
#include <utility>

using std::string;
using std::unordered_map;
using std::cout;
using std::endl;
using std::vector;
using std::pair;

bool can_be_palindrome(const string& x) {
  unordered_map<char, int> letters;

  for (char letter : x) {
    int count = letters[letter];
    letters[letter] = count + 1;
  }

  int num_odds = 0;
  for (pair<char, int> entry : letters) {
    if (entry.second % 2 == 1) {
      num_odds++;
    }
    if (num_odds > 1) {
      return false;
    }
  }
  return true;
}

int main(void) {
  vector<string> tests
    {
     "ABB",
     "ABCBCA",
     "",
     "AAA",
     "ABC",
     "AAAB",
     "AAABB"
    };

  for (string test : tests) {
    cout << test << ": " << (can_be_palindrome(test) ? "yes" : "no")
         << endl;
  }

  return 0;
}
