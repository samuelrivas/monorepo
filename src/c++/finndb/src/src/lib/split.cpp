// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include "split.hpp"

#include <vector>
#include <string>

using std::vector;
using std::string;

vector<string> split(const string& in, char sep) {
  int start = 0;
  vector<string> out;

  for (size_t i = 0; i < in.size(); i++) {
    if (in[i] == sep) {
      out.push_back(in.substr(start, i - start));
      start = i + 1;
    }
  }
  out.push_back(in.substr(start, in.size() - start));
  return out;
}

