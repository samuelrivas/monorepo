/* Copyright 2020 samuelrivas@gmail.com
 */

#include <string>
#include <thread>

#include "shared-cache.hpp"

using std::string;
using std::thread;

using sam::SharedCache;

int main() {

  SharedCache<string, string> cache;

  auto write_foo =
    [&cache]() {
      cache.insert("foo key", "foo value", 1);
    };
  auto write_bar =
    [&cache]() {
      cache.insert("bar key", "bar value", 2);
    };
  auto write_baz =
    [&cache]() {
      cache.insert("baz key", "baz value", 3);
    };

  auto read_foo =
    [&cache]() {
      cache.lookup("foo key");
    };
  auto read_bar =
    [&cache]() {
      cache.lookup("bar key");
    };
  auto read_baz =
    [&cache]() {
      cache.lookup("baz key");
    };

  thread write1(write_foo);
  thread write2(write_bar);
  thread write3(write_baz);

  write1.join();
  write2.join();
  write3.join();

  thread read3(read_foo);
  thread read4(read_bar);
  thread write4(write_foo);
  thread read5(read_baz);

  thread read6(read_foo);
  thread read7(read_bar);
  thread read8(read_baz);

  read3.join();
  read4.join();
  write4.join();
  read5.join();
  read6.join();
  read7.join();
  read8.join();

  cerr.flush();
  return 0;
}
