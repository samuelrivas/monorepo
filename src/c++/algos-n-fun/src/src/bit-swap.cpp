/* Copyright 2018 <samuelrivas@gmail.com> */

#include <cstdint>
#include <iostream>
#include <iomanip>

uint32_t swap_bits(uint32_t x) {
  uint32_t odd_mask = 0x55555555;
  uint32_t even_mask = 0xAAAAAAAA;

  return ((x >> 1) & odd_mask) | ((x << 1) & even_mask);
}

int main(void) {
  uint32_t test = 0x93;

  std::cout << std::setbase(16) << "0x" << test << " -> ";
  std::cout << std::setbase(16) << "0x" << swap_bits(test) << std::endl;

  return 0;
}
