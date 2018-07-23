//Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <string>
#include <sstream>
#include <boost/format.hpp>

#include "sql-lines.hpp"
#include "sha1.hpp"

using std::string;
using std::ostringstream;
using boost::format;

namespace internal {
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
}

using internal::to_fixed_point;

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
