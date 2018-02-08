/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Just a simple way of reversing a string a-la C
 */
#include <iostream>
#include <string>
#include <utility>

using std::cout;
using std::endl;
using std::string;
using std::swap;

void reverse_string(string* a) {
  int len;

  for (len = 0; (*a)[len] != '\0'; len++) {}

  for (int i = len - 1; i > len - i - 1; i--) {
    swap((*a)[i], (*a)[len - i - 1]);
  }
}

int main(void) {
  string test1("foo");
  string test2("the quick brown fox");
  string test3("abcd");

  reverse_string(&test1);
  reverse_string(&test2);
  reverse_string(&test3);
  cout << test1 << endl;
  cout << test2 << endl;
  cout << test3 << endl;

  return 0;
}
