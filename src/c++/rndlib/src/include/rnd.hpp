// Copyright (C) 2019 by Samuel Rivas <samuelrivas@gmail.com>

#ifndef __RND_HPP__
#define __RND_HPP__

namespace sam {
  /* Get a random integer from /dev/urandom to be used as seed in in a PSRNG */
  bool get_seed(int *seed);
}

#endif
