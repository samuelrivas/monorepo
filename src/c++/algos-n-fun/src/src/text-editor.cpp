#include <iostream>
#include <string>
#include <vector>
#include <cassert>

#include "lib/rabin-karp.hpp"

using std::vector;
using std::cout;
using std::cerr;
using std::cin;
using std::endl;
using std::string;

// Play wild and ignore false positives for maximal speed
int subsequences(const string& text, int start, int max_width, int width) {
  if (max_width == width) {
    return 1;
  }
  assert(max_width > width);

  RKHasher<'z' - 'a' + 1> hasher(width);
  vector<bool> seen(hasher.get_modulus(), false);
  int total = 0;

  for (int i = 0; i < width - 1; i++) {
    hasher.push(text[start + i] - 'a');
  }

  for (int i = width - 1; i < max_width; i++) {
    int hash = hasher.push(text[start + i] - 'a');
    if (!seen[hash]) {
      seen[hash] = true;
      total++;
    }
  }
  return total;
}

int total_subsequences(const string& text, int start, int max_width) {
  int total = 0;

  for (int seq_length = 1; seq_length <= max_width; seq_length++) {
    int subseq = subsequences(text, start, max_width, seq_length);
    total += subseq;
  }
  return total;
}

int main(void) {
  string text;
  cin >> text;

  int n_questions, width;
  cin >> n_questions;
  cin >> width;

  for (int n_question = 0; n_question < n_questions; n_question++) {
    int start;
    cin >> start;
    start--;
    cout << total_subsequences(text, start, width) << endl;
  }
  return 0;
}
