/* Copyright 2017 samuelrivas@gmail.com
 *
 * The transaction id is the sha1 of the UTF-8 encoding of the full csv line
 * being translated, without the newline character
 *
 * The movement id is the sha1 of the all the original used fields concatenated
 * using semicolons to separate them. Check the code below for more details
 * (look for sha1 calls)
 */

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>
#include <boost/format.hpp>

#include "lib/sha1.hpp"

constexpr int FIX_POINT_PRECISION = 10;

using std::cout;
using std::cerr;
using std::cin;
using std::string;
using std::vector;
using std::ostringstream;
using boost::format;

vector<string> split(const string& in, char sep) {
  int start = 0;
  vector<string> out;

  for (size_t i = 0; i < in.size(); i++) {
    if (in[i] == sep) {
      out.push_back(in.substr(start, i - start));
      start = i + 1;
    }
  }
  out.push_back(in.substr(start, in.size() - start));
  return out;
}

// We use 10 decimals fixed point for amounts
string to_fixed_point(const string& amount) {
  ostringstream out;
  bool after_comma = false;
  int trailing_zeroes = FIX_POINT_PRECISION;

  for (char c : amount) {
    if (c == ',') {
      after_comma = true;
    } else {
      out << c;
      if (after_comma) {
        trailing_zeroes--;
      }
    }
  }

  assert(trailing_zeroes >= 0);

  while (trailing_zeroes-- > 0) {
    out << '0';
  }
  return out.str();
}

string transaction_line(const string& transaction_id,
                        const string& date,
                        const string& raw_line) {
  ostringstream out;
  out << "INSERT INTO transactions (id, date, raw) VALUES "
      << format("(\"%s\",\"%s\",\"%s\");\n")
    % transaction_id
    % date
    % raw_line;

  return out.str();
}

string movement_line(const string& date,
                     const string& asset,
                     const string& account,
                     const string& amount,
                     const string& transaction_id) {
  format id_format("%s;%s;%s;%s;%s");
  string movement_id = sha1(str(id_format % date % asset % account % amount
                                % transaction_id));
  ostringstream out;
  out << "INSERT INTO movements "
    "(id, date, asset, account, amount, decimals, trans_id) VALUES "

      << format("(\"%s\",\"%s\",\"%s\",\"%s\",%s,%s,\"%s\");\n")
    % movement_id
    % date
    % asset
    % account
    % to_fixed_point(amount)
    % FIX_POINT_PRECISION
    % transaction_id;

  return out.str();
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, ';');

    if (tokens.size() != 10) {
      cerr << format("Line '%s' produces %d tokens, we want 10\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    string transaction_id = sha1(line);

    // Transaction
    cout << transaction_line(transaction_id, tokens[0], line);

    // Cash movement
    cout << movement_line(tokens[0], tokens[8], tokens[1],
                          tokens[6], transaction_id);

    if (tokens[2] == "Köp" || tokens[2] == "Sälj") {
      // Asset movement
      cout << movement_line(tokens[0], tokens[3], tokens[1], tokens[4],
                            transaction_id);
    }
  }
  return 0;
}
