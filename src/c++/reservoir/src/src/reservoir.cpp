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

  void push(const T x) {
    std::size_t pos { count };
    if (pos >= size) {
      // Get a chance to replace an existing element
      std::uniform_int_distribution<int> distribution(0, count);
      pos = distribution(random_engine);
    }

    if (pos < size) {
      samples.at(pos) = x;
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

bool get_seed(int *seed) {
  std::ifstream dev_random("/dev/random", std::ios::binary);
  dev_random.read(reinterpret_cast<char *>(seed), sizeof(int));

  if (dev_random.fail()) {
    std::cerr << "Failed to get random seed: " << strerror(errno) << "\n";
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
  if (!get_seed(&seed)) {
    return EXIT_FAILURE;
  }

  Reservoir<std::string> reservoir(size, seed);

  for (std::string line; std::getline(std::cin, line);) {
    reservoir.push(line);
  }

  for (auto i : reservoir) {
    std::cout << i << "\n";
  }
  return 0;
}
