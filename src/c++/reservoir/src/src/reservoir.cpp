/* Copyright 2017 samuelrivas@gmail.com
 *
 * Sample a fixed number of input lines from an infinite stream of lines
 */

#include <time.h>

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>
#include <random>
#include <sstream>
#include <string>
#include <vector>

#include <rnd.hpp>

template <typename T>
class Reservoir {
  std::vector<T> samples;
  std::size_t size;
  std::size_t count;
  std::default_random_engine random_engine;

 public:
  using const_iterator = typename std::vector<T>::const_iterator;

  explicit Reservoir(std::size_t _size, int seed) :
    samples { std::vector<T>(_size) },
    size { _size },
    count { 0 },
    random_engine {
        static_cast<std::default_random_engine::result_type>(seed)
          }
  { }

  // TODO(Samuel): Figure out these later
  Reservoir(const Reservoir &that) = delete;
  Reservoir& operator=(const Reservoir &) = delete;

  /* We assume that we will call push much more often than the size of the
     reservoir, In such scenario, items will be copied into the samples vector
     infrequently. Thus, we choose to pass the items as const references and pay
     the penalty of a sure copy when inserting. For large datasets, this is
     faster than the alternative of pssing by value and moving into the samples
     vector */
  void push(const T& x) {
    std::size_t pos { count };
    if (pos >= size) {
      // Get a chance to replace an existing element
      std::uniform_int_distribution<int> distribution(0, count);
      pos = distribution(random_engine);
    }

    if (pos < size) {
      samples.at(pos) = T(x);
    }
    count++;
  }

  const_iterator begin() {
    return samples.begin();
  }

  const_iterator end() {
    return begin() + std::min(size, count);
  }
};

void print_usage(const std::string name) {
  std::cerr << "Read lines from standard input until it is closed.\n"
            << "Then output <n> samples.\n\n"
            << "Usage:\n"
            << name << " <n>\n";
}

template <typename T>
bool parse_int(const std::string text, T *out) {
  // Bad error handling, but less hassle than any other option I know
  std::stringstream ss(text);
  ss >> *out;
  if (ss.fail()) {
    return false;
  }
  return true;
}

bool parse_command(int argc, char* argv[], size_t *size) {
  if (argc != 2) {
    print_usage(argv[0]);
    return false;
  }

  if (!parse_int<size_t>(argv[1], size)) {
    std::cerr <<  argv[1] << " must be an integer\n";
    return false;
  }
  return true;
}

int main(int argc, char* argv[]) {
  std::size_t size;

  if (!parse_command(argc, argv, &size)) {
    return EXIT_FAILURE;
  }

  int seed;
  if (!sam::get_seed(&seed)) {
    return EXIT_FAILURE;
  }

  Reservoir<std::string> reservoir(size, seed);

  std::cin.sync_with_stdio(false);

  for (std::string line; std::getline(std::cin, line);) {
    reservoir.push(line);
  }

  for (auto i : reservoir) {
    std::cout << i << "\n";
  }
  return 0;
}
