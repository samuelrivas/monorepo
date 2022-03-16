//Copyright (C) 2022 by Samuel Rivas <samuelrivas@gmail.com>

/* This loads a tsv from a spreadsheet with columns for date, asset, account,
   bank, amount and transaction type. We currently set transaction type to misc,
   because we are not using it for anything and we are too lazy to add better
   logic */

#include <string>
#include <vector>
#include <iostream>
#include <libgen.h>
#include <boost/format.hpp>

#include "lib/sha1.hpp"
#include "lib/sql-lines.hpp"
#include "lib/split.hpp"

using std::string;
using std::cout;
using std::cerr;
using std::cin;
using std::vector;
using boost::format;

TransactionType parse_type(const string& type_text) {
  if (type_text == "cash_transfer") {
    return TransactionType::CASH_TRANSFER;
  } else if (type_text == "tax") {
    return TransactionType::TAX;
  } else if (type_text == "interest") {
    return TransactionType::INTEREST;
  }

  cerr << format("%s is not yet supported as transaction type\n") % type_text;
  std::flush(cerr);
  assert(false);
}


int main() {
  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, '\t');

    if (tokens.size() != 6) {
      cerr << format("Line '%s' produces %d tokens, we want 6\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }
    string transaction_id = sha1(line);
    cout << transaction_line(transaction_id, tokens[0], parse_type(tokens[5]), line);
    cout << movement_line(tokens[0], tokens[1], tokens[2], tokens[3], tokens[4], transaction_id);
  }
  return 0;
}
