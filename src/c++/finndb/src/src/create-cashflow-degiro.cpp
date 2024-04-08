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

bool register_line(const string& description_text, TransactionType* type) {
  if (description_text.find("Comisión de conectividad") != string::npos) {
      *type = TransactionType::FEE;
      return true;
  }
  if (description_text == "Dividendo") {
    *type = TransactionType::DIVIDEND;
    return true;
  }
  if (description_text == "Ingreso") {
    *type = TransactionType::CASH_TRANSFER;
    return true;
  }
  if (description_text == "Ingreso Cambio de Divisa") {
    *type = TransactionType::MISC;
    return true;
  }
  if (description_text == "Interés") {
    *type = TransactionType::INTEREST;
    return true;
  }
  if (description_text == "Rendimiento de capital") {
    *type = TransactionType::INTEREST;
    return true;
  }
  if (description_text == "Retención del dividendo") {
    *type = TransactionType::TAX;
    return true;
  }
  if (description_text == "Retirada") {
    *type = TransactionType::CASH_TRANSFER;
    return true;
  }
  if (description_text == "Retirada Cambio de Divisa") {
    *type = TransactionType::MISC;
    return true;
  }
  if (description_text.find("Acciones corporativas") != string::npos) {
    *type = TransactionType::MISC;
    return true;
  }
  return false;
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, '\t');

    if (tokens.size() != 12) {
      cerr << format("Line '%s' produces %d tokens, we want 12\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    TransactionType type;

    if (register_line(tokens[5], &type)) {
      string transaction_id = sha1(line);
      string date = rearrange_date(tokens[0]);
      cout << transaction_line(transaction_id, date, type, line);
      cout << movement_line(date, tokens[7], "degiro-es", "degiro", tokens[8],
                            transaction_id);

    } else {
      cerr << "Not registering: " << tokens[5] << std::endl;
    }
  }
  return 0;
}
