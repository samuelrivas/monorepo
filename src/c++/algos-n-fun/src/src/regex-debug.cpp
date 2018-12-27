#include <iostream>
#include <string>
#include <regex>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::getline;
using std::string;
using std::regex;
using std::regex_match;
using std::smatch;
using std::ssub_match;
using std::sregex_iterator;

int main(void) {
  cin.sync_with_stdio(false);

  string line;
  getline(cin, line);

  for (string regex_str; getline(cin, regex_str);) {
    regex regex(regex_str);
    smatch matches;
    if (regex_match(line, matches, regex)) {
      cout << "Matches: '" << matches[0] << "'" << endl;
      cout << "Submatches:" << endl;
      for (ssub_match sm : matches) {
        cout << "- '" << sm << "'" << endl;
      }
    } else {
      cout << "Nope!" << endl;
    }
  }
  return 0;
}
