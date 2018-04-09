/* Copyright 2018 <samuelrivas@gmail.com> */

#include <cstdint>
#include <cassert>
#include <sstream>
#include <string>

using std::ostringstream;
using std::string;

bool get_bit(const uint32_t& x, int pos) {
  assert(0 <= pos && pos < 32);

  return (x & (1 << (31 - pos))) != 0;
}

uint32_t set_bit(const uint32_t& x, int pos, bool bit) {
  assert(0 <= pos && pos < 32);

  if (bit) {
    return x | (1 << (31 - pos));
  } else {
    return x & ~(1 << (31 - pos));
  }
}

string to_s(uint32_t x) {
  ostringstream out;
  for (int i = 0; i < 32; i++) {
    out << (get_bit(x, i) ? '1' : '0');
  }
  return out.str();
}

uint32_t move_to_right(uint32_t x, bool bit) {
  int trailing = 0;
  size_t pos = 31;

  for (; pos < 32 && get_bit(x, pos) == bit; trailing ++, pos --) { }

  for (; pos < 32 && get_bit(x, pos) != bit; pos --) { }

  assert(pos < 32);
  assert(get_bit(x, pos) == bit);

  x = set_bit(x, pos++, !bit);
  x = set_bit(x, pos++, bit);
  for (; trailing > 0; trailing--, pos++) {
    x = set_bit(x, pos, bit);
  }

  for (; pos < 32; pos++) {
    x = set_bit(x, pos, !bit);
  }
  return x;
}

uint32_t find_next(uint32_t x) {
  return move_to_right(x, false);
}

uint32_t find_previous(uint32_t x) {
  return move_to_right(x, true);
}

#include <vector>
#include <iostream>
#include <iomanip>

using std::vector;
using std::cout;
using std::endl;
using std::setbase;

int main(void) {
  vector<uint32_t> tests = { 0x084C, 0xB3, 0x1C, 0x23 };

  for (uint32_t test : tests) {
    cout << "test: " << to_s(test) << endl;
    cout << "next: " << to_s(find_next(test)) << endl;
    cout << "prev: " << to_s(find_previous(test)) << endl;
    cout << endl;
  }
  return 0;
}
