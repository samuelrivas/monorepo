// Copyright (C) 2018 by samuelrivas@gmail.com

#include <vector>
#include <iostream>
#include <iomanip>
#include <cassert>
#include <string>

using std::vector;
using std::cout;
using std::setw;
using std::endl;
using std::string;

size_t trim_right(const vector<string>& buff, size_t pos) {
  for ( ; pos <= buff.size() && buff[pos] == ""; pos--) { }
  return pos <= buff.size() ? pos : 0;
}

size_t trim_left(const vector<string>& buff, size_t pos) {
  for ( ; pos < buff.size() && buff[pos] == ""; pos++) { }
  return pos < buff.size() ? pos : buff.size() - 1;
}

int find(const vector<string>& buff, const string& x) {
  size_t r = trim_right(buff, buff.size() - 1);
  size_t l = trim_left(buff, 0);

  while (l <= r) {
    size_t untrimmed_m = l + (r - l) / 2;
    size_t m_left = trim_left(buff, untrimmed_m);
    size_t m_right = trim_right(buff, untrimmed_m);

    if (buff[m_left] == x) {
      return m_left;
    } else if (buff[m_right] == x) {
      return m_right;
    }

    if (x < buff[m_left]) {
      r = trim_right(buff, m_left - 1);
    } else {
      l = trim_left(buff, m_right + 1);
    }
  }
  return -1;
}

int main(void) {
  string x = "foo";
  vector<vector<string>> tests = {
    { "at", "", "", "", "ball", "", "", "car", "", "", "dad", "", "" },
    { "at", "", "", "", "ball", "", "", "car", "", "", "dad", "x", "", "z" },
    { "at", "", "", "", "foo", "", "", "xar", "", "", "zad", "", "" },
    { "", "at", "", "foo", "", "", "xar", "", "", "zad", "", "" },
    { "", "a", "", "", "", "b", "", "", "", "c", "", "" },
    { "", "", "", "", "", "", "", "", "" }
  };

  for (vector<string> test : tests) {
    string separator = "";
      for (string s : test) {
        cout << separator << s;
        separator = ", ";
      }
    cout << endl;
    int pos = find(test, x);
    cout << pos << " : ";

    if (pos == -1) {
      cout << "XX";
    } else {
      cout << test[pos];
    }

    cout << endl;
  }
}
