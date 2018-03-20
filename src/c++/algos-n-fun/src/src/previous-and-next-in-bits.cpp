#include <array>
#include <cstdint>
#include <utility>
#include <cassert>
#include <sstream>
#include <string>

using std::array;
using std::move;
using std::ostringstream;
using std::string;

#include <vector>
#include <iostream>
#include <iomanip>

using std::vector;
using std::cout;
using std::endl;
using std::setbase;

typedef array<bool, 32> BitArray;

BitArray to_bit_array(uint32_t x) {
  BitArray result;

  for (int i = 31; i >= 0; i--) {
    result[i] = x % 2 == 1;
    x /= 2;
  }
  return result;
}

uint32_t from_bit_array(const BitArray& x) {
  uint32_t result = 0;
  for (int i = 0; i < 32; i++) {
    if (x[i]) {
      result += 1 << (31 - i);
    }
  }
  return result;
}

string to_s(const BitArray& x) {
  ostringstream out;
  for (int i = 0; i < 32; i++) {
    out << (x[i] ? '1' : '0');
  }
  return out.str();
}

// We need to copy, x so we just do it right away passing by value
BitArray move_to_right(BitArray x, bool bit) {
  BitArray result = move(x);

  int trailing = 0;
  size_t pos = 31;

  for (; pos < 32 && result[pos] == bit; trailing ++, pos --) { }

  for (; pos < 32 && result[pos] != bit; pos --) { }

  assert(pos < 32);
  assert(result[pos] == bit);

  result[pos++] = !bit;
  result[pos++] = bit;
  for (; trailing > 0; trailing--, pos++) {
    result[pos] = bit;
  }

  for (; pos < 32; pos++) {
    result[pos] = !bit;
  }
  return result;
}

uint32_t find_next(uint32_t x) {
  return from_bit_array(move_to_right(to_bit_array(x), false));
}

uint32_t find_previous(uint32_t x) {
  return from_bit_array(move_to_right(to_bit_array(x), true));
}

int main(void) {
  vector<uint32_t> tests = { 0x084C, 0xB3, 0x1C, 0x23 };

  for (uint32_t test : tests) {
    BitArray bits = to_bit_array(test);
    cout << "test: " << to_s(bits) << endl;
    cout << "next: " << to_s(to_bit_array(find_next(test))) << endl;
    cout << "prev: " << to_s(to_bit_array(find_previous(test))) << endl;
    cout << endl;
  }
  return 0;
}