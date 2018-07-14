/* Copyright 2017 samuelrivas@gmail.com
 *
 * Add a sha1 at the end of each line, separated with a semicolon (this is
 * hardcoded for now)
 */

#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <boost/format.hpp>

#include "lib/sha1.hpp"

using std::cout;
using std::cerr;
using std::cin;
using std::string;
using std::vector;
using boost::format;

vector<string> split(const string& in, char sep) {
  int start = 0;
  vector<string> out;

  for (size_t i = 0; i < in.size(); i++) {
    if (in[i] == sep) {
      out.push_back(in.substr(start, i - start));
      start = i + 1;
    }
  }
  out.push_back(in.substr(start, in.size() - start));
  return out;
}

// We assume that the cash amount is always xxxx.xx
string convert_amount(const string& amount) {
  vector<string> tokens = split(amount, ',');
  if (tokens.size() != 2 || tokens[1].size() != 2) {
    cerr << format("'%s' violates the expected amount format\n")
      % amount;
    for (string s : tokens) {
      cout << s << std::endl;
    }
    std::flush(cerr);
    assert(false);
  }

  return tokens[0] + tokens[1];
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, ';');

    if (tokens.size() != 10) {
      cerr << format("Line '%s' produces %d tokens, we want 10\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    cout << "insert into movements"
         << " (id, date, account, asset, amount, decimals) values "
         << format("(\"%s\",\"%s\",\"%s\",\"%s\",%s,2);\n")
      % sha1(line)
      % tokens[0]
      % tokens[1]
      % tokens[8]
      % convert_amount(tokens[6]);

  }
  return 0;
}
