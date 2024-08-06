#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>
#include <limits>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::pair;
using std::ostringstream;
using std::istringstream;
using std::string;
using std::max;
using std::min;
using std::getline;
using std::terminate;
using std::numeric_limits;
using std::hex;
using std::find;
using std::set;

string format_recipes(const vector<int>& recipes, size_t elf_1, size_t elf_2) {
  ostringstream out;

  for (size_t i = 0; i < recipes.size(); i++) {
    if (elf_1 == i) {
      out << "(" << recipes[i] << ")";
    } else if (elf_2 == i) {
      out << "[" << recipes[i] << "]";
    } else {
      out << " " << recipes[i] << " ";
    }
  }
  return out.str();
}

int main(void) {
  cin.sync_with_stdio(false);
  vector<int> recipes {3, 7};
  size_t elf_1 = 0;
  size_t elf_2 = 1;

  size_t n_recipes;
  cin >> n_recipes;

  // cerr << format_recipes(recipes, elf_1, elf_2) << endl;

  while (recipes.size() < n_recipes + 10) {
    int combined = recipes[elf_1] + recipes[elf_2];
    if (combined / 10 > 0) {
      recipes.push_back(combined / 10);
    }
    recipes.push_back(combined % 10);

    elf_1 = (elf_1 + recipes[elf_1] + 1) % recipes.size();
    elf_2 = (elf_2 + recipes[elf_2] + 1) % recipes.size();

    // cerr << format_recipes(recipes, elf_1, elf_2) << endl;
  }
  cout << "Solution: ";
  for (size_t i  = n_recipes; i < n_recipes + 10; i++) {
    cout << recipes[i];
  }
  cout << endl;
  return 0;
}
