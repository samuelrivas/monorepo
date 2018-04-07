// Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>
#include <cstdint>
#include <ios>

using std::cout;
using std::endl;
using std::hex;

int main(void) {
  uint32_t number = 0x12345678;
  uint8_t* bytes = reinterpret_cast<uint8_t*>(&number);
  printf("%02X %02X %02X %02X\n", bytes[3], bytes[2], bytes[1], bytes[0]);
  return 0;
}
