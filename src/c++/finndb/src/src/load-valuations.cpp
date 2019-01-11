/* Copyright 2017 samuelrivas@gmail.com
 *
 * Load from a googlesheet export. We require
 *  - tsv format
 *  - A locale where decimals are separated by commas
 */

#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <unordered_map>
#include <boost/format.hpp>

#include "lib/sql-lines.hpp"
#include "lib/split.hpp"

using std::cout;
using std::cerr;
using std::cin;
using std::string;
using std::vector;
using boost::format;

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, '\t');

    if (tokens.size() != 5) {
      cerr << format("Line '%s' produces %d tokens, we want 5\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    cout << valuation_line(tokens[0], tokens[1], tokens[3]);
  }
}
