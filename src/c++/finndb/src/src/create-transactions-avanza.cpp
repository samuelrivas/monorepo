/* Copyright 2017 samuelrivas@gmail.com
 *
 * The transaction id is the sha1 of the UTF-8 encoding of the full csv line
 * being translated, without the newline character
 *
 * The movement id is the sha1 of the all the original used fields concatenated
 * using semicolons to separate them. Check the code below for more details
 * (look for sha1 calls)
 *
 * The input is a csv with the following format:
 *
 * 0    ,1     ,2                ,3                      ,4    ,5   ,6     ,7                 ,8             ,9         ,10              ,11  ,12
 * Datum;Konto;Typ av transaktion;Värdepapper/beskrivning;Antal;Kurs;Belopp;Transaktionsvaluta;Courtage (SEK);Valutakurs;Instrumentvaluta;ISIN;Resultat
 */

#include <iostream>
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
using boost::format;

// At some point avanza removed types, so we need to infer them here, but
// currently the only "Övrigt" types are taxes
TransactionType parse_misc(const string& description) {
  if (description.find("skatt") != string::npos) {
    return TransactionType::TAX;
  } else {
    cerr << format("Cannot infer transaction type for description '%s'\n")
      % description;
    std::flush(cerr);
    assert(false);
  }
}

// This is quite fragile, so always fail in case of unknown types
TransactionType parse_type(const string& type_text, const string& description) {
  if (type_text ==  "Köp") {
    return TransactionType::BUY;
  } else if (type_text == "Sälj") {
    return TransactionType::SELL;
  } else if (type_text == "Preliminärskatt"
             || type_text == "Utländsk källskatt") {
    return TransactionType::TAX;
  } else if (type_text == "Ränta") {
    return TransactionType::INTEREST;
  } else if (type_text == "Utdelning") {
    return TransactionType::DIVIDEND;
  } else if (type_text == "Insättning"
             || type_text == "Uttag") {
    return TransactionType::CASH_TRANSFER;
  } else if (type_text == "Värdepappersöverföring") {
    return TransactionType::ASSET_TRANSFER;
  } else if (type_text == "Övrigt") {
    return parse_misc(description);
  } else {
    cerr << format("Cannot parse transaction type '%s' with description '%s'\n")
      % type_text
      % description;
    std::flush(cerr);
    assert(false);
  }
}

string isin_to_asset(const string& isin) {
  if (isin == "SE0000993560") {
    return "ÖHMAN REALOBLIGATIONSFOND";
  }
  if (isin == "SE0001472952") {
    return "Öhman Realräntefond A";
  }
  if (isin == "NO0010736879") {
    return "Schibsted B";
  }
  cerr << format("Unknown ISIN '%s'\n")
    % isin;
  std::flush(cerr);
  assert(false);
}

int main() {

  cin.sync_with_stdio(false);

  for (string line; std::getline(cin, line);) {
    vector<string> tokens = split(line, ';');

    if (tokens.size() != 13) {
      cerr << format("Line '%s' produces %d tokens, we want 13\n")
        % line % tokens.size();
      std::flush(cerr);
      assert(false);
    }

    string transaction_id = sha1(line);
    TransactionType type = parse_type(tokens[2], tokens[3]);

    // Transaction
    cout << transaction_line(transaction_id, tokens[0], type, line);

    // Cash movement
    //
    // Movements that introduce stock from other accounts show up as Övrigt,
    // with no cash movement
    if (type != TransactionType::ASSET_TRANSFER || tokens[10] != "-") {
      cout << movement_line(tokens[0], tokens[10], tokens[1], "Avanza",
                            tokens[6], transaction_id);
    }

    // Movements and transactions
    if (type == TransactionType::BUY
        || type == TransactionType::SELL) {
      // Asset movement
      cout << movement_line(tokens[0], tokens[3], tokens[1], "Avanza",
                            tokens[4], transaction_id);

    } else if (type == TransactionType::ASSET_TRANSFER) {
      // This is also an asset movement, but Avanza codifies the asset name in
      // the description field. For now we hardcode the assets using the ISIN,
      // but we may want to move to always use the ISIN and have another table
      // to translate ISINs to asset names
      cout << movement_line(tokens[0], isin_to_asset(tokens[11]), tokens[1], "Avanza",
                            tokens[4], transaction_id);
    } else {

      // Just some safety verifications
      if (type != TransactionType::DIVIDEND
          && type != TransactionType::TAX) {
        // Dividend and tax have an asset amount set, but they don't involve
        // changing any asset holding (they just affect cash). Anything here
        // should relate to no assets and thus "Antal" should be ""
        if (tokens[4] != "") {
          cerr << "Found a potential asset movement that we don't expect:\n"
               << "antal is \"" << tokens[4] << "\" we want \"-\"\n"
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
