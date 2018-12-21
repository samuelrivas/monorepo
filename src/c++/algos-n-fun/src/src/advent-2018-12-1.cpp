#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <list>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <regex>
#include <exception>
#include <limits>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::list;
using std::vector;
using std::pair;
using std::ostringstream;
using std::istringstream;
using std::string;
using std::setw;
using std::max;
using std::min;
using std::getline;
using std::regex;
using std::smatch;
using std::regex_match;
using std::terminate;
using std::numeric_limits;
using std::hex;

const size_t GENERATIONS = 20;
const size_t STRIDE_SIZE = 5;

vector<bool> parse_pots(const string& line) {
  // Pad with empty pots to the left and to the right, in practice they won't
  // grow much more than a pot or two per generation to either side

  vector<bool> pots(STRIDE_SIZE/2 * GENERATIONS, false);

  for (size_t i = 15; i < line.size(); i++) {
    pots.push_back(line[i] == '#');
  }
  for (size_t i = 0; i < STRIDE_SIZE/2 * GENERATIONS; i++) {
    pots.push_back(false);
  }
  return pots;
}

string format_pots(const vector<bool>& pots) {
  ostringstream out;
  size_t count = 0;
  for (bool pot : pots) {
    if (count++ == STRIDE_SIZE/2 * GENERATIONS) {
      out << "<<>>";
    }
    out << (pot ? '#' : '.');
  }
  return out.str();
}

pair<int, bool> parse_notebook_entry(const string& line) {
  int entry_encoding = 0;

  for (size_t i = 0; i < STRIDE_SIZE; i++) {
    entry_encoding <<= 1;
    entry_encoding |= (line[i] == '#');
  }
  return { entry_encoding, line[STRIDE_SIZE + 4] == '#' };
}

int push_pot(int current_code, bool pot) {
  return ((current_code & ((1 << (STRIDE_SIZE - 1)) - 1)) << 1) | pot;
}

int main(void) {
  cin.sync_with_stdio(false);

  string line;
  getline(cin, line);
  vector<bool> pots = parse_pots(line);

  getline(cin, line); // empty

  vector<bool> notebook(1 << STRIDE_SIZE);
  while(getline(cin, line)) {
    pair<int, bool> entry = parse_notebook_entry(line);
    notebook.at(entry.first) = entry.second;
  }

  // for (size_t i = 0; i < notebook.size(); i++) {
  //   cerr << hex << i << ": " << (notebook.at(i) ? '#' : '.') << endl;
  // }

  cerr << "Generation  0" << ": "
       << format_pots(pots) << endl;

  for (int generation = 1; generation <= 20; generation++) {
    int entry_encoding = 0;
    size_t read_head = 0;

    // We have enough padding not to worry about special cases for the sides, so
    // we just initialise the read head to the whole stride
    for (; read_head < STRIDE_SIZE - 1; read_head++) {
      entry_encoding = push_pot(entry_encoding, pots[read_head]);
    }

    size_t write_head = STRIDE_SIZE/2;

    for (; read_head < pots.size(); read_head++) {
      entry_encoding = push_pot(entry_encoding, pots[read_head]);
      // cerr << read_head <<  " entry_encoding: " << entry_encoding
      //      << " in: " << pots[write_head]
      //      << " out: " << notebook.at(entry_encoding) << endl;
      pots[write_head++] = notebook.at(entry_encoding);
    }
    cerr << "Generation " << setw(2) << generation << ": "
         << format_pots(pots) << endl;
  }

  int score = 0;
  for (size_t i = 0; i < pots.size(); i++) {
    if (pots[i]) {
      score += static_cast<int>(i - STRIDE_SIZE/2 * GENERATIONS);
    }
  }

  cout << "Solution: " << score << endl;
  return 0;
}
