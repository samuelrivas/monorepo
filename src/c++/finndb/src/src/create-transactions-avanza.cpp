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

enum class TransactionType {
  BUY,
  SELL,
  ASSET_TRANSFER,
  CASH_TRANSFER,
  TAX,
  INTEREST,
  DIVIDEND,
  MISC
};

// This is quite fragile, so always fail in case of unknown types
TransactionType parse_type(const string& type_text) {
  if (type_text ==  "Köp") {
    return TransactionType::BUY;
  } else if (type_text == "Sälj") {
    return TransactionType::SELL;
  } else if (type_text == "Preliminärskatt"
             || type_text.find("Utländsk källskatt") == 0) {
    return TransactionType::TAX;
  } else if (type_text == "Räntor") {
    return TransactionType::INTEREST;
  } else if (type_text == "Utdelning") {
    return TransactionType::DIVIDEND;
  } else if (type_text == "Insättning"
             || type_text == "Uttag") {
    return TransactionType::CASH_TRANSFER;
  } else if (type_text.find("Övf från ") == 0
             || type_text.find("Byte till ") == 0
             || type_text.find("Byte från ") == 0) {
    return TransactionType::ASSET_TRANSFER;
  } else if (type_text == "Övrigt") {
    return TransactionType::MISC;
  } else {
    cerr << format("Cannot parse transaction type '%s'\n")
      % type_text;
    std::flush(cerr);
    assert(false);
  }
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
    cout << movement_line(tokens[0], tokens[8], tokens[1], "Avanza",
                          tokens[6], transaction_id);

    TransactionType type = parse_type(tokens[2]);

    // Movements and transactions
    if (type == TransactionType::BUY
        || type == TransactionType::SELL
        || type == TransactionType::ASSET_TRANSFER) {
      // Asset movement
      cout << movement_line(tokens[0], tokens[3], tokens[1], "Avanza",
                            tokens[4], transaction_id);
    } else {

      // Just some safety verifications
      if (type != TransactionType::DIVIDEND
          && type != TransactionType::TAX) {
        // Dividend and tax have an asset amount set, but they don't involve
        // changing any asset holding (they just affect cash). Anything here
        // should relate to no assets and thus "Antal" should be "-"
        if (tokens[4] != "-") {
          cerr << "Found a potential asset movement that we don't expect:\n"
               << line;
          assert(false);
        }
      }
    }

    // Valuations
    if (type == TransactionType::BUY
        || type == TransactionType::SELL) {
      cout << valuation_line(tokens[0], tokens[3], tokens[5]);
    }
  }
  return 0;
}
