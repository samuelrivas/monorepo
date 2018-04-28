#include <queue>
#include <utility>
#include <iostream>
#include <string>
#include <vector>

#include "lib/rabin-karp.hpp"

using std::vector;
using std::queue;
using std::pair;
using std::cout;
using std::endl;
using std::string;

int main(void) {
  vector<pair<string, string>> tests {
    { "the quick (and lazy) brown fox jumps over the lazy dog", "lazy" },
    { "abracadabra", "ra" },
    { "foofo", "foo" },
    { "aaaaaaaaaaa", "aaaaa" },
    { "~~~~~~~~~~~", "~~~~~" } // checking overflow
  };

  for (pair<string, string> test : tests) {
    string text = test.first;
    string query = test.second;

    cout << "'" << query << "' in '" << text << "'" << endl;

    RKHasher<128> query_hasher(query.size());
    for (char c : query) {
      query_hasher.push(c);
    }

    int query_hash = query_hasher.get_hash();

    cout << "query hash: " << query_hash << endl;

    RKHasher<128> text_hasher(query.size());

    for (size_t pos = 0; pos < text.size(); pos++) {
      int text_hash = text_hasher.push(text[pos]);

      cout << "Hash at " << pos << ": " << text_hash;

      if (text_hash == query_hash) {
        cout << " ! (" << pos - query.size() + 1 << ", " << pos << ")";
      }
      cout << endl;
    }
  }
  return 0;
}
