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
 * For now, we use the account csv (turned into a tsv, and . translated
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

string unquote(const string& in) {
  assert(in.size() > 2);
  return in.substr(1, in.size() - 2);
}

string rearrange_date(const string& in) {
  vector<string> tokens = split(in, '-');
  ostringstream out;
  assert(tokens.size() == 3);
  out << format("%s-%s-%s") % tokens[2] % tokens[1] % tokens[0];
  return out.str();
}

bool register_line(const string& description_text) {
  return description_text.find("Compra") == string::npos
    && description_text.find("Venta") == string::npos
    && description_text.find("Conversón Cash Fund") == string::npos
    && description_text.find("Cash Fund cambio de precio") == string::npos
    && description_text != "Comisión de compra/venta";
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, '\t');

    if (tokens.size() != 11) {
      cerr << format("Line '%s' produces %d tokens, we want 11\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    if (register_line(tokens[5])) {
      string transaction_id = sha1(line);
      string date = rearrange_date(tokens[0]);
      cout << transaction_line(transaction_id, date, line);
      cout << movement_line(date, tokens[7], "degiro-es", "degiro", tokens[8],
                            transaction_id);

    }
  }
  return 0;
}
