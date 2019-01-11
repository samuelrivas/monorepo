//Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <string>
#include <sstream>
#include <boost/format.hpp>

#include "sql-lines.hpp"
#include "sha1.hpp"
#include "assert_msg.hpp"

using std::string;
using std::ostringstream;
using boost::format;

namespace internal {
  string to_fixed_point(const string& amount) {
    ostringstream out;
    bool after_comma = false;
    bool valid_minus = true;
    int trailing_zeroes = FIX_POINT_PRECISION;

    for (char c : amount) {
      if (c == '-' && !valid_minus) {
        assert_msg(format("Found a - in the middle of an alleged number: 's'")
                   % amount,
                   false);
      } else if (c == ',') {
        assert(!after_comma);
        after_comma = true;
      } else {
        assert_msg(format("'%c' is a digit (from '%s')") % c % amount,
                   c == '-' || (c >= '0' && c <= '9'));
        out << c;
        if (after_comma) {
          trailing_zeroes--;
        }
      }
      valid_minus = false;
    }

    assert(trailing_zeroes >= 0);

    while (trailing_zeroes-- > 0) {
      out << '0';
    }
    return out.str();
  }
}

using internal::to_fixed_point;

string type_to_string(const TransactionType& type) {
  switch (type) {
  case TransactionType::BUY:
    return "buy";
  case TransactionType::SELL:
    return "sell";
  case TransactionType::CASH_TRANSFER:
    return "cash_transfer";
  case TransactionType::TAX:
    return "tax";
  case TransactionType::INTEREST:
    return "interest";
  case TransactionType::DIVIDEND:
    return "dividend";
  case TransactionType::FEE:
    return "fee";
  case TransactionType::ASSET_TRANSFER:
  case TransactionType::MISC:
  default:
    return "misc";
  }
}

string transaction_line(const string& transaction_id,
                        const string& date,
                        const TransactionType& type,
                        const string& raw_line) {
  ostringstream out;
  out << "INSERT INTO transactions (id, date, type, raw) VALUES "
      << format("(\"%s\",\"%s\",\"%s\",\"%s\");\n")
    % transaction_id
    % date
    % type_to_string(type)
    % raw_line;

  return out.str();
}

string movement_line(const string& date,
                     const string& asset,
                     const string& account,
                     const string& bank,
                     const string& amount,
                     const string& transaction_id) {
  format id_format("%s;%s;%s;%s;%s");
  string movement_id = sha1(str(id_format % date % asset % account % amount
                                % transaction_id));
  ostringstream out;
  out << "INSERT INTO movements "
    "(id, date, asset, account, bank, amount, decimals, trans_id) VALUES "
      << format("(\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%s,%s,\"%s\");\n")
    % movement_id
    % date
    % asset
    % account
    % bank
    % to_fixed_point(amount)
    % FIX_POINT_PRECISION
    % transaction_id;

  return out.str();
}

string valuation_line(const string& date,
                      const string& asset,
                      const string& amount) {
  ostringstream out;
  out << "INSERT INTO valuations (date, asset, amount, decimals) VALUES "
      << format("(\"%s\",\"%s\",%s,%s);\n")
    % date
    % asset
    % to_fixed_point(amount)
    % FIX_POINT_PRECISION;

  return out.str();
}
