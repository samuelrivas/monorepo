/* Copyright 2017 samuelrivas@gmail.com
 *
 * Add a sha1 at the end of each line, separated with a semicolon (this is
 * hardcoded for now)
 */

#include <iostream>
#include <sstream>
#include <string>
#include <array>
#include <boost/format.hpp>
#include <boost/uuid/detail/sha1.hpp>

using std::cout;
using std::cin;
using std::string;
using std::array;
using std::ostringstream;
using boost::format;

// Abusing boost a bit to get a reasonable hash function without extra
// dependencies
string sha1(const string& s) {
  boost::uuids::detail::sha1 sha1;
  sha1.process_bytes(s.data(), s.size());
  unsigned hash[5] = { };
  sha1.get_digest(hash);

  ostringstream out;

  for (int i = 0; i < 5; i++) {
    out << format("%08x") % hash[i];
  }

  return std::string(out.str());
}

int main() {

  cin.sync_with_stdio(false);

  sha1("foo");

  for (std::string line; std::getline(cin, line);) {
    cout << format("%s;%s\n") % line % sha1(line);
  }
  return 0;
}
