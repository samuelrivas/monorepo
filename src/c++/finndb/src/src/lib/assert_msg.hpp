// Copyright (C) 2018 by samuelrivas@gmail.com
#ifndef __ASSERT_MSG_HPP__
#define __ASSERT_MSG_HPP__

#include <boost/format.hpp>

void assert_msg(const boost::format& msg, bool cond);

#endif
