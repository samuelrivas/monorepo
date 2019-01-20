#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <sstream>
#include <algorithm>
#include <exception>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <queue>
#include <regex>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::pair;
using std::ostringstream;
using std::istringstream;
using std::string;
using std::max;
using std::min;
using std::getline;
using std::terminate;
using std::numeric_limits;
using std::hex;
using std::find;
using std::set;
using std::unordered_set;
using std::unordered_map;
using std::queue;
using std::map;
using std::regex;
using std::regex_match;
using std::smatch;

class FourInts {
private:
  vector<int> values;
public:
  FourInts(int a, int b, int c, int d) :
    values { a, b, c, d } { };

  FourInts(vector<int> v) :
    values(v) {
    assert(v.size() == 4);
  }

  int operator[](int index) const {
    return values.at(index);
  }

  bool operator==(const FourInts& b) const {
    return values == b.as_v();
  }

  vector<int> as_v() const {
    return values;
  }

  string to_s() const {
    ostringstream out;
    out << values[0] << ", "
        << values[1] << ", "
        << values[2] << ", "
        << values[3];

    return out.str();
  }
};

typedef FourInts Registers;
typedef FourInts Op;

struct Sample {
  Registers before, after;
  Op op;

  Sample(const Registers& _before, const Op& _op, const Registers& _after) :
    before { _before },
    after { _after },
    op { _op } { };
};

class OpHandler {
private:
  virtual Registers handler(vector<int> registers, const Op& op) const = 0;
public:
  virtual string name() const = 0;
  const Registers operator()(const Registers& in, const Op& op) const {
   return  handler(in.as_v(), op);
  }
};

class Addr : public OpHandler {
  string name() const { return "addr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] + registers[op[2]];
    return registers;
  }
} addr;

class Addi : public OpHandler {
  string name() const { return "addi"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] + op[2];
    return registers;
  }
} addi;

class Mulr : public OpHandler {
  string name() const { return "mulr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] * registers[op[2]];
    return registers;
  }
} mulr;

class Muli : public OpHandler {
  string name() const { return "muli"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] * op[2];
    return registers;
  }
} muli;

class Banr : public OpHandler {
  string name() const { return "banr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] & registers[op[2]];
    return registers;
  }
} banr;

class Bani : public OpHandler {
  string name() const { return "bani"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] & op[2];
    return registers;
  }
} bani;

class Borr : public OpHandler {
  string name() const { return "borr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] | registers[op[2]];
    return registers;
  }
} borr;

class Bori : public OpHandler {
  string name() const { return "bori"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] | op[2];
    return registers;
  }
} bori;

class Setr : public OpHandler {
  string name() const { return "setr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]];
    return registers;
  }
} setr;

class Seti : public OpHandler {
  string name() const { return "seti"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = op[1];
    return registers;
  }
} seti;

class Gtir : public OpHandler {
  string name() const { return "gtir"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = op[1] > registers[op[2]];
    return registers;
  }
} gtir;

class Gtri : public OpHandler {
  string name() const { return "gtri"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] > op[2];
    return registers;
  }
} gtri;

class Gtrr : public OpHandler {
  string name() const { return "gtrr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] > registers[op[2]];
    return registers;
  }
} gtrr;

class Eqir : public OpHandler {
  string name() const { return "eqir"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = op[1] == registers[op[2]];
    return registers;
  }
} eqir;

class Eqri : public OpHandler {
  string name() const { return "eqri"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] == op[2];
    return registers;
  }
} eqri;

class Eqrr : public OpHandler {
  string name() const { return "eqrr"; };

  Registers handler(vector<int> registers, const Op& op) const {
    registers[op[3]] = registers[op[1]] == registers[op[2]];
    return registers;
  }
} eqrr;

vector<OpHandler*> op_handlers { &addr, &addi,
                                 &mulr, &muli,
                                 &banr, &bani,
                                 &borr, &bori,
                                 &setr, &seti,
                                 &gtir, &gtri, &gtrr,
                                 &eqir, &eqri, &eqrr
};

// Surely, there must be a better way of doing this
int match_to_int(const string& match) {
  istringstream istr(match);
  int out;
  istr >> out;
  return out;
}

Registers parse_registers(const string line) {
  smatch matches;
  regex registers_regex("(Before|After): *"
                        "\\[([[:digit:]]), ([[:digit:]]), "
                        "([[:digit:]]), ([[:digit:]])\\]");

  if (!regex_match(line, matches, registers_regex)) {
    cerr << "This line doesn't match as registers: " << line << endl;
    terminate();
  }

  return { match_to_int(matches[2]),
           match_to_int(matches[3]),
           match_to_int(matches[4]),
           match_to_int(matches[5]) };
}

Op parse_op(const string line) {
  smatch matches;
  regex registers_regex("([[:digit:]]+) ([[:digit:]]) "
                        "([[:digit:]]) ([[:digit:]])");

  if (!regex_match(line, matches, registers_regex)) {
    cerr << "This line doesn't match as op: " << line << endl;
    terminate();
  }

  return { match_to_int(matches[1]),
           match_to_int(matches[2]),
           match_to_int(matches[3]),
           match_to_int(matches[4]) };
}

Sample parse_sample() {
  string before;
  string op;
  string after;

  if (!getline(cin, before) || before.size() == 0
      || !getline(cin, op) || op.size() == 0
      || !getline(cin, after) || after.size() == 0) {
    abort();
  }

  // Skip one white line
  cin.ignore(1);

  smatch matches;

  return Sample(parse_registers(before), parse_op(op), parse_registers(after));
}

int main(void) {
  cin.sync_with_stdio(false);

  vector<Sample> samples;

  while (cin.peek() != '\n') {
    samples.push_back(parse_sample());
  }

  int count = 0;
  for (Sample sample : samples) {
    vector<string> matches;
    for (OpHandler* h : op_handlers) {
      Registers after = (*h)(sample.before, sample.op);

      if (after == sample.after) {
        matches.push_back(h -> name());
      }

      if (matches.size() >= 3) {
        count++;

        cerr << "Before: " << sample.before.to_s() << endl;
        cerr << "After : " << sample.after.to_s() << endl;
        cerr << "Op: " << sample.op.to_s() << endl;
        cerr << "Matches: ";
        for (string match : matches) {
          cerr << match << " ";
        }
        cerr << endl << endl;
        break;
      }
    }
  }

  cout << "Solution: " << count << endl;
  return 0;
}
