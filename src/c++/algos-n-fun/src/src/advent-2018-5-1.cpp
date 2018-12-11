#include <iostream>
#include <string>
#include <cassert>
#include <forward_list>
#include <cstdlib>
#include <sstream>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::forward_list;
using std::ostringstream;

string format_polymer(const forward_list<char>& polymer) {
  ostringstream out;
  for (char c : polymer) {
    out << c;
  }
  return out.str();
}

int main(void) {
  string input;
  forward_list<char> polymer;
  // Assuming no spaces, but that is fine for this exercise
  cin >> input;
  for (size_t i = input.size() - 1; i < input.size(); i --) {
    polymer.push_front(input[i]);
  }

  bool changed = true;

  // This can be faster by keeping a lower bound of what part of the ploymer can
  // react (e.g. if we find a reaction in position 10, we only need to start
  // over from position 9. This finishes in my computer though so I wouldn't
  // bother 
  while (!polymer.empty() && changed) {

    forward_list<char>::iterator before = polymer.before_begin();
    forward_list<char>::iterator pos = polymer.begin();
    forward_list<char>::iterator after = polymer.begin();
    after++;
    changed = false;

    while (!changed && after != polymer.end()) {
      if (abs(*pos - *after) == 'a' - 'A') {
        cerr << "Annihilation!: " << *pos << *after << endl;

        polymer.erase_after(pos);
        polymer.erase_after(before);

        changed = true;
        cerr << format_polymer(polymer) << endl;
      } else {
        before++;
        pos++;
        after++;
      }
    }
  }

  cout << format_polymer(polymer) << endl;
  return 0;
}
