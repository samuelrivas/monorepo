//Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#ifndef __SQL_LINES_HPP__
#define __SQL_LINES_HPP__

#include <string>

using std::string;

constexpr int FIX_POINT_PRECISION = 10;

enum class TransactionType {
  BUY,
  SELL,
  ASSET_TRANSFER,
  CASH_TRANSFER,
  TAX,
  INTEREST,
  DIVIDEND,
  FEE,
  MISC
};

string type_to_string(const TransactionType& type);

string transaction_line(const string& transaction_id,
                        const string& date,
                        const TransactionType& type,
                        const string& raw_line);
string movement_line(const string& date,
                     const string& asset,
                     const string& account,
                     const string& bank,
                     const string& amount,
                     const string& transaction_id);
string valuation_line(const string& date,
                      const string& asset,
                      const string& amount);
#endif
