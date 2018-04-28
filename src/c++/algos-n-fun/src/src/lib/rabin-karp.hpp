#ifndef _RABIN_KARP_H_
#define _RABIN_KARP_H_

#include <queue>
#include <cstddef>

/* Be careful not to use big radixes without increasing the modulus or you may
   overflow and get incorrect results. However, don't pick too big of a modulus,
   or you will overflow too.

   Needless to say, modulus must be prime.
*/
template<int radix, int modulus = 933739>
class RKHasher {
  size_t size;
  std::queue<int> digits;
  long hash = 0; // careful with not overflowing this
  int outPow;

 public:
  explicit RKHasher(size_t _size) :
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

#endif
