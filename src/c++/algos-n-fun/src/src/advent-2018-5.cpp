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

forward_list<char> filter_element(string input, char to_filter) {
  forward_list<char> polymer;

  for (size_t i = input.size() - 1; i < input.size(); i --) {
    if (input[i] != to_filter && input[i] != to_filter + ('a' - 'A')) {
      polymer.push_front(input[i]);
    }
  }
  return polymer;
}

void react_polymer(forward_list<char>* polymer) {
  bool changed = true;

  while (!(polymer -> empty()) && changed) {

    forward_list<char>::iterator before = polymer -> before_begin();
    forward_list<char>::iterator pos = polymer -> begin();
    forward_list<char>::iterator after = polymer -> begin();
    after++;
    changed = false;

    while (!changed && after != polymer -> end()) {
      if (abs(*pos - *after) == 'a' - 'A') {
        // cerr << "Annihilation!: " << *pos << *after << endl;

        polymer -> erase_after(pos);
        polymer -> erase_after(before);

        changed = true;
        // cerr << format_polymer(polymer) << endl;
      } else {
        before++;
        pos++;
        after++;
      }
    }
  }
}

int main(void) {
  cin.sync_with_stdio(false);
  string input;
  // Assuming no spaces, but that is fine for this exercise
  cin >> input;

  forward_list<char> polymer;
  for (size_t i = input.size() - 1; i < input.size(); i --) {
    polymer.push_front(input[i]);
  }

  react_polymer(&polymer);

  string final_polymer = format_polymer(polymer);
  cout << final_polymer << endl;
  cout << "That had " << final_polymer.size() << " characters" << endl;

  size_t min_length = final_polymer.size() + 1;
  for (char element = 'A'; element <= 'Z'; element++) {

    forward_list<char> filtered_polymer = filter_element(input, element);
    react_polymer(&filtered_polymer);
    string filtered_result = format_polymer(filtered_polymer);

    if (filtered_result.size() < min_length) {
      min_length = filtered_result.size();
      cerr << "Filtering " << element << " we get a nice result of " << min_length << endl;
    }
  }

  cout << "Second part: " << min_length << endl;
  return 0;
}
