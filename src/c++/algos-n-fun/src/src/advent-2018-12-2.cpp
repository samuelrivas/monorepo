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

const size_t GENERATIONS = 50000000000;
const size_t LOOKAHEAD_GENERATIONS = 200;
const size_t STRIDE_SIZE = 5;

vector<bool> parse_pots(const string& line) {

  const int SKIP_CHARS = 15; //"Initial state: "
  int initial_size = static_cast<int>(line.size()) - SKIP_CHARS;

  // For large sizes we cannot use push_back, as the doubling nature of the
  // array may blow our memory. Thus we calculate the static size here:
  //
  // We pad with empty pots to the left and to the right, in practice they won't
  // grow much more than a pot per generation to either side
  vector<bool> pots(2 * LOOKAHEAD_GENERATIONS + initial_size, false);

  for (size_t i = SKIP_CHARS; i < line.size(); i++) {
    pots[i - SKIP_CHARS + LOOKAHEAD_GENERATIONS] =  line[i] == '#';
  }
  return pots;
}

string format_pots(const vector<bool>& pots) {
  ostringstream out;
  size_t count = 0;
  for (bool pot : pots) {
    if (count++ == LOOKAHEAD_GENERATIONS) {
      out << "<<>>";
    }
    out << (pot ? '#' : '.');
  }
  return out.str();
}

string format_just_flowers(const vector<bool>& pots) {
  ostringstream out;
  size_t pos = 0;

  while(!pots[pos++]);
  pos--;

  size_t last_pot = pos - 1;
  for (size_t i = pos; i < pots.size(); i++) {
    if (pots[i]) {
      last_pot = i;
    }
  }

  for(; pos <= last_pot; pos++) {
    out << (pots[pos] ? '#' : '.');
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

pair<long,long> flower_interval(const vector<bool>& pots) {

  long first_flower = -LOOKAHEAD_GENERATIONS;
  size_t i = 0;
  while(!pots[i]) {
    first_flower++;
    i++;
  }

  long last_flower = first_flower;
  for (; i < pots.size(); i++) {
    if (pots[i]) {
      last_flower = i - LOOKAHEAD_GENERATIONS;
    }
  }
  return { first_flower, last_flower };
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

  string just_flowers = format_just_flowers(pots);
  pair<long,long> interval = flower_interval(pots);
  cerr << "Generation   0" << ": "
       << just_flowers << endl;
  cerr << interval.first << "," << interval.second << endl;

  long displacement = 0;
  size_t generation = 1;
  bool converged = false;
  while (!converged) {
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
      pots[write_head++] = notebook.at(entry_encoding);
    }
    string new_flowers = format_just_flowers(pots);
    cerr << "Generation " << setw(3) << generation << ": "
         << new_flowers << endl;

    pair<long,long> new_interval = flower_interval(pots);
    cerr << new_interval.first << "," << new_interval.second << endl;

    if (new_flowers == just_flowers) {
      cerr << "We have converged!" << endl;
      displacement = new_interval.first - interval.first;
      converged = true;
    } else {
      generation++;
    }
    just_flowers = new_flowers;
    interval = new_interval;
  }

  long first_score = interval.first + (GENERATIONS - generation) * displacement;
  cerr << "Converged in generation " << generation << endl;
  cerr << "interval " << interval.first << "," << interval.second << endl;
  cerr << "Displacement " << displacement << endl;
  cerr << "Finally, the first pot will be " << first_score << endl;

  long score = 0;
  long pot_score = first_score;
  for (long i = interval.first + static_cast<long>(LOOKAHEAD_GENERATIONS);
       i <= interval.second + static_cast<long>(LOOKAHEAD_GENERATIONS);
       i++, pot_score++) {
    if (pots[i]) {
      score += pot_score;
    }
  }

  cout << "Solution: " << score << endl;
  return 0;
}
