#include <queue>
#include <utility>
#include <iostream>
#include <string>
#include <vector>

using std::vector;
using std::queue;
using std::pair;
using std::cout;
using std::endl;
using std::string;

/* Be careful not to use big radixes without increasing the modulus or you may
   overflow and get incorrect results. However, don't pick too big of a modulus,
   or you will overflow too.

   Needless to say, modulus must be prime.
*/
template<int radix, int modulus = 933739>
class RKHash {
  size_t size;
  queue<int> digits;
  long hash = 0; // careful with not overflowing this
  int outPow;

 public:
  explicit RKHash(size_t _size) :
    size { _size } {
    outPow = 1;
    for (size_t i = 0; i < size - 1; i++) {
      outPow = (outPow * radix) % modulus ;
    }
  }

  int push(int x) {
    int out = 0;
    if (digits.size() == size) {
      out = digits.front();
      digits.pop();
    }

    digits.push(x);
    hash = (radix * (hash - out * outPow) + x) % modulus;

    if (hash < 0) {
      hash += modulus;
    }

    return hash;
  }

  int get_hash() {
    return hash;
  }
};

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

    RKHash<128> query_hasher(query.size());
    for (char c : query) {
      query_hasher.push(c);
    }

    int query_hash = query_hasher.get_hash();

    cout << "query hash: " << query_hash << endl;

    RKHash<128> text_hasher(query.size());

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
