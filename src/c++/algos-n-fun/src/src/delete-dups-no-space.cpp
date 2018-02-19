/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Delete duplicates in a vector using no extra space
 */
#include <vector>
#include <iostream>
#include <string>

using std::vector;
using std::cout;
using std::endl;
using std::string;

void delete_dups(char* a) {
  int hi_mark = 0;

  for (int i = 0; a[i] != 0; i++) {
    bool dup = false;
    for (int j = 0; j < hi_mark && !dup; j++) {
      if (a[i] == a[j]) {
        dup = true;
      }
    }

    if (!dup) {
      a[hi_mark] = a[i];
      hi_mark++;
    }
  }
  a[hi_mark] = 0;
}

int main(void) {
  char test1[] = "foo";
  char test2[] = "the quick brown fox jumps over the lazy dog";
  char test3[] = "abcd";
  char test4[] = "";
  char test5[] = "abababacd";
  char test6[] = "aaaaaaaaa";
  char test7[] = "aaaaaaaaaf";
  char test8[] = "aaaaaaaaaffffff";

  vector<char*> tests {
    test1,
      test2,
      test3,
      test4,
      test5,
      test6,
      test7,
      test8
      };

  for (auto test : tests) {
    cout << test << " -> ";
    delete_dups(test);
    cout << test << endl;
  }

  return 0;
}
