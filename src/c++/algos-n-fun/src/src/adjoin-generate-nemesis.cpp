/* Copyright (C) 2018 by samuelrivas@gmail.com
 *
 * This generates a testcase that brings down the current adjoin solution
 */
#include <iostream>

using std::cout;
using std::endl;

int main(void) {
  cout.sync_with_stdio(false);

  cout << "100000 99999" << endl;
  for (int i = 0; i + 1 + i/2 < 100000; i++) {
    cout << i + 1 + i/2 << " " << i/2 * 3 << endl;
  }
  return 0;
}
