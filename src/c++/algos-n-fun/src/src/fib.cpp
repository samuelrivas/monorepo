/* Copyright 2018 <samuelrivas@gmail.com> */
#include <cassert>
#include <iostream>
#include <iomanip>

using std::cout;
using std::setw;
using std::endl;

int fib(int n) {
  assert(n >= 0);

  // fib_1 is fib(n-1), starting at 1 in a clever priouette
  int fib_1 = 1;
  int fib = 0;

  for (int i = 0; i < n; i++) {
    int old_fib = fib;
    fib += fib_1;
    fib_1 = old_fib;
  }

  return fib;
}

int main(void) {
  for (int i = 0; i < 10; i++) {
    cout << setw(3) << fib(i);
  }
  cout << endl;
  return 0;
}
