// Copyright (C) 2019 by Samuel Rivas <samuelrivas@gmail.com>

#include <fstream>
#include <iostream>
#include <cstring>

using std::ifstream;
using std::cerr;

#include "../include/rnd.hpp"

bool sam::get_seed(int *seed) {
  ifstream dev_random("/dev/urandom", std::ios::binary);
  dev_random.read(reinterpret_cast<char*>(seed), sizeof(int));

  if (dev_random.fail()) {
    std::cerr << "Failed to get random seed: " << strerror(errno) << "\n";
    return false;
  }

  return true;
}
