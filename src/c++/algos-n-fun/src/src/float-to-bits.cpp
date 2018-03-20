/* Copyright 2018 <samuelrivas@gmail.com> */
#include <string>
#include <sstream>
#include <cassert>
#include <forward_list>
#include <iostream>

using std::string;
using std::stoi;
using std::ostringstream;
using std::forward_list;
using std::cout;
using std::endl;

string integer_as_bits(int x) {
  forward_list<char> out;
  while (x > 0) {
    out.push_front(x % 2 == 1 ? '1' : '0');
    x /= 2;
  }
  return string(out.cbegin(), out.cend());
}

string double_as_bits(double x, size_t maxbits) {
  ostringstream out;

  while (x != 0 && maxbits > 0) {
    cout << x << endl;
    x *= 2;
    if (x >= 1) {
      x -= 1;
      out << '1';
    } else {
      out << '0';
    }
    maxbits--;
  }

  if (x != 0) {
    cout << "det går inte att konvertera!" << endl;
    exit(1);
  }
  return out.str();
}

string as_bits(const string& n) {
  size_t dot = n.find(".");

  assert(dot != string::npos);

  int integer = stoi(n.substr(0, dot));
  double decimal = stod(n.substr(dot));

  ostringstream out;

  out << integer_as_bits(integer)
      << ".";

  out << double_as_bits(decimal, 32 - out.tellp());

  return out.str();
}

int main(int argc, char *argv[]) {
  string input;
  if (argc != 2) {
    std::cout << "Defaulting to 3.141593" << std::endl;
    input = "3.141593";
  } else {
    input = argv[1];
  }
  std::cout << input << " -> ";
  std::cout << as_bits(input) << std::endl;
}
