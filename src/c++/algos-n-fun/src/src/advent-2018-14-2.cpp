#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>

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

size_t digits(size_t n) {
  size_t result = 1;
  while (n / 10 > 0) {
    result++;
    n /= 10;
  }
  return result;
}

size_t divisor(size_t n) {
  size_t result = 1;
  while (n--) {
    result *= 10;
  }
  return result;
}

size_t push(size_t n, size_t divisor, size_t digit) {
  return (n * 10) % divisor + digit;
}

int main(void) {
  cin.sync_with_stdio(false);
  vector<int> recipes {3, 7};
  size_t elf_1 = 0;
  size_t elf_2 = 1;

  size_t target;
  cin >> target;

  // This won't work if there are zeroes at the beginning, but there aren't in
  // my input, so meh
  size_t _digits = digits(target);
  size_t _divisor = divisor(_digits);

  size_t head = 37;

  while (true) {
    int combined = recipes[elf_1] + recipes[elf_2];
    if (combined / 10 > 0) {
      recipes.push_back(combined / 10);
      head = push(head, _divisor, combined / 10);

      if (head == target) {
        cerr << "Found!" << endl;
        // cerr << format_recipes(recipes, elf_1, elf_2) << endl;
        break;
      }
    }
    recipes.push_back(combined % 10);
    head = push(head, _divisor, combined % 10);
    if (head == target) {
      cerr << "Found!" << endl;
      cerr << format_recipes(recipes, elf_1, elf_2) << endl;
      break;
    }

    elf_1 = (elf_1 + recipes[elf_1] + 1) % recipes.size();
    elf_2 = (elf_2 + recipes[elf_2] + 1) % recipes.size();
    // cerr << format_recipes(recipes, elf_1, elf_2) << endl;
    // cerr << head << endl;
  }
  cout << "Solution: " << recipes.size() - _digits << endl;
  return 0;
}
