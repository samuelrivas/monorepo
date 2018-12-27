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

  regex integer("^(\\+|-)?[[:digit:]]+$");
  regex words("^[[:space:]]*([[:alnum:]]+[[:space:]]*)+$");

  smatch matches;
  for (string line; getline(cin, line);) {
    if (regex_match(line, matches, integer)) {
      cout << "That was an integer: " << matches[0] << endl;
    } else if (regex_match(line, matches, words)) {
      cout << "That was a bunch of words: " << matches[0] << endl;
    } else {
      cout << "Don't know what you mean..." << endl;
    }
    cout << "Submatches:" << endl;
    for (ssub_match sm : matches) {
      cout << sm << endl;
    }

    cout << "With iterator" << endl;
    regex word("[[:alnum:]]+");
    for (sregex_iterator i = sregex_iterator(line.begin(), line.end(), word);
         i != sregex_iterator();
         i++) {
      cout << " ** '" << i -> str() << "'" << endl;
    }
  }
  return 0;
}
