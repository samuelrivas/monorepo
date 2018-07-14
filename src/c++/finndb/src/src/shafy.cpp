/* Copyright 2017 samuelrivas@gmail.com
 *
 * Add a sha1 at the end of each line, separated with a semicolon (this is
 * hardcoded for now)
 */

#include <iostream>
#include <string>
#include <boost/format.hpp>

#include "lib/sha1.hpp"

using std::cout;
using std::cin;
using std::string;
using boost::format;

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    cout << format("%s;%s\n") % line % sha1(line);
  }
  return 0;
}
