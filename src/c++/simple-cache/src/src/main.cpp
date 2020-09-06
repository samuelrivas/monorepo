/* Copyright 2020 samuelrivas@gmail.com
 *
 * Simple, expiring cache
 */

#include <time.h>

#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <utility>

#include "scache.hpp"

using std::cout;
using std::cerr;
using std::endl;
using std::unordered_map;
using std::priority_queue;
using std::pair;
using std::string;


using sam::SCache;

int main() {
  SCache<int, string> cache;

  cache.insert(10, "foo", 0);
  cache.insert(20, "bar", 3);
  cache.insert(10, "fooo", 4);
  cache.insert(30, "kuux", 2);

  // cache.insert(10, 0, 0);
  // cache.insert(20, 1, 0);
  // cache.insert(10, 2, 0);
  // cache.insert(30, 3, 0);

  cout << cache.lookup(10).value().first << endl;
  cout << cache.lookup(20).value().first << endl;
  cout << cache.lookup(30).value().first << endl;

  if (cache.lookup(40).has_value()) {
    cout << "40 should not have value, WAAAT" << endl;
  }

  cout << "flushing 3" << endl;
  cache.flush(3);
  cout << "flushing 10" << endl;
  cache.flush(10);
  return 0;
}
