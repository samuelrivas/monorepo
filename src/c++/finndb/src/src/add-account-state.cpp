//Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

/* This adds a single line transaction to setup the current state of an
   asset. It is meant to be used for banks where getting full history of
   transactions in a feasible format is not possible, so that a base state can
   be set by hand */

#include <string>
#include <iostream>
#include <libgen.h>
#include <boost/format.hpp>

#include "lib/sha1.hpp"
#include "lib/sql-lines.hpp"

using std::string;
using std::cout;
using std::cerr;
using boost::format;

int main(int argc, char* argv[]) {

  if (argc != 6) {
    cerr << "Say what!?\n";
    return 1;
  }

  (void) argc;
  (void) argv;

  string date { argv[1] };
  string bank { argv[2] };
  string account { argv[3] };
  string asset = { argv[4] };
  string amount = { argv[5] };

  string raw_line = (format("generated by %s;%s;%s;%s;%s;%s")
                     % basename(argv[0])
                     % date
                     % bank
                     % account
                     % asset
                     % amount).str();

  string transaction_id = sha1(raw_line);
  cout << transaction_line(transaction_id, date, raw_line);
  cout << movement_line(date, asset, account, bank, amount, transaction_id);
  return 0;
}
