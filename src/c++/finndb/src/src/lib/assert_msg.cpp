// Copyright (C) 2018 by samuelrivas@gmail.com

#include <cstdlib>
#include <iostream>
#include <boost/format.hpp>

#include "assert_msg.hpp"

void assert_msg(const boost::format& msg, bool cond) {
  if (!cond) {
    std::cerr << "\n ** Assertion failed: " << msg << std::endl;
    abort();
  }
}
