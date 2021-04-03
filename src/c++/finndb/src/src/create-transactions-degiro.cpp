/* Copyright 2017 samuelrivas@gmail.com
 *
 * Degiro uses a csv with commas as separator (unlike avanza which uses
 * semicolons). That complicates our code so far, so we convert them first to
 * tsv, to avoid the complication of parsing numbers with commas as decimal
 * separator (as they are econded within quotes to avoid conflicts with the
 * outer commas, that separate fields)
 *
 * The transaction id is the sha1 of the UTF-8 encoding of the full csv line
 * being translated, without the newline character
 *
 * The movement id is the sha1 of the all the original used fields concatenated
 * using semicolons to separate them. Check the code below for more details
 * (look for sha1 calls)
 *
 * For now, we use the transactions csv (turned into a tsv, and . translated
 * into , afterwards)
 */

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>
#include <unordered_map>
#include <boost/format.hpp>

#include "lib/sha1.hpp"
#include "lib/sql-lines.hpp"
#include "lib/split.hpp"

using std::cout;
using std::cerr;
using std::cin;
using std::string;
using std::vector;
using std::ostringstream;
using boost::format;

string rearrange_date(const string& in) {
  vector<string> tokens = split(in, '-');
  ostringstream out;
  assert(tokens.size() == 3);
  out << format("%s-%s-%s") % tokens[2] % tokens[1] % tokens[0];
  return out.str();
}

TransactionType get_type(const string& asset_amount) {
  if (asset_amount[0] == '-') {
    return TransactionType::SELL;
  } else {
    return TransactionType::BUY;
  }
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, '\t');

    if (tokens.size() != 19) {
      cerr << format("Line '%s' produces %d tokens, we want 19\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    string transaction_id = sha1(line);
    string date = rearrange_date(tokens[0]);
    cout << transaction_line(transaction_id, date, get_type(tokens[6]), line);

    // Asset movement
    cout << movement_line(date, tokens[2], "degiro-es", "degiro", tokens[6],
                          transaction_id);

    // Cash movement
    cout << movement_line(date, tokens[17], "degiro-es", "degiro", tokens[16],
                          transaction_id);
    // cout << valuation_line(date, tokens[2], tokens[8]);
  }
  return 0;
}
