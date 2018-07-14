#include <string>
#include <cstdint>
#include <sstream>
#include <boost/uuid/detail/sha1.hpp>
#include <boost/format.hpp>

#include "sha1.hpp"

using std::string;
using std::ostringstream;
using boost::format;

// Abusing boost a bit to get a reasonable hash function without extra
// dependencies
string sha1(const string& s) {
  boost::uuids::detail::sha1 sha1;
  sha1.process_bytes(s.data(), s.size());

  uint32_t hash[5] = { };
  sha1.get_digest(hash);

  ostringstream out;
  for (int i = 0; i < 5; i++) {
    out << format("%08x") % hash[i];
  }

  return std::string(out.str());
}
