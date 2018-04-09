/* Copyright 2018 <samuelrivas@gmail.com> */

#include <cstdint>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>

using std::cout;
using std::setbase;
using std::setfill;
using std::setw;
using std::endl;
using std::string;
using std::ostringstream;

string to_s(uint32_t n) {
  ostringstream oss;
  uint32_t mask = 0x000000FF;
  uint32_t b0 = (n >> 24) & mask;
  uint32_t b1 = (n >> 16) & mask;
  uint32_t b2 = (n >>  8) & mask;
  uint32_t b3 =  n        & mask;

  oss << setbase(16) << setfill('0')
      << setw(2) << b0 << " "
      << setw(2) << b1 << " "
      << setw(2) << b2 << " "
      << setw(2) << b3 << " ";

  return oss.str();
}

int32_t blend(int32_t n, int32_t m, int i, int j) {
  assert(i >= 0 && i < 32);
  assert(j >= 0 && j < 32);
  assert(i <= j);

  int32_t mask = j - i == 31
    ? ~0
    : (1 << (j - i + 1)) - 1;

  mask <<= (32 - j - 1);
  cout << "mask: " << to_s(mask) << endl;

  return (n & ~mask) | (m & mask);
}

int main() {
  int32_t a = 0x3F3F3F3F;
  int32_t b = 0xAAAAAAAA;
  int32_t result = blend(a, b, 8, 23);

  cout << "a   : " << to_s(a) << endl;
  cout << "b   : " << to_s(b) << endl;
  cout << "res : " << to_s(result) << endl;

  return 0;
}
