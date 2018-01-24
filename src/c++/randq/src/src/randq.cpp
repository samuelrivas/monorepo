/* Copyright 2017 samuelrivas@gmail.com
 *
 * Random queue implementation and command line interface
 */

#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>
#include <random>
#include <string>

// TODO(Samuel): set this to something bigger when we have a large enough test
constexpr int kInitialSize = 4;

template <typename T>
class RandQ {

  int size, allocated_size;
  T* elements;
  std::default_random_engine random_engine;

public:
  RandQ (int seed) :
    size { 0 },
    allocated_size { kInitialSize },
    elements { new T[kInitialSize] },
    random_engine {
        static_cast<std::default_random_engine::result_type>(seed)
          }
  { }

  ~RandQ () {
    delete[] elements;
  }

  // TODO(Samuel): Figure out these later
  RandQ(const RandQ &that) = delete;
  RandQ& operator=(const RandQ &) = delete;

  void push (T element) {
    if (allocated_size == size) {
      grow();
    }

    assert(allocated_size > size);
    elements[size] = element;
    size++;
  }

  bool pop(T *out) {
    if (size == 0) {
      return false;
    }

    int pos = random_pos();
    *out = elements[pos];
    elements[pos] = elements[size - 1];
    size --;

    if (size <= allocated_size/4) {
      shrink();
    }
    return true;
  }

private:
  void grow() {
    assert(allocated_size * 2 > allocated_size);
    allocated_size *= 2;
    realloc_and_copy();
  }

  void shrink() {
    if (allocated_size / 2 < kInitialSize) {
      return;
    };

    assert(allocated_size / 2 >= size);
    allocated_size /= 2;
    realloc_and_copy();
  }

  void realloc_and_copy() {

    assert(size <= allocated_size);

    T* new_elements = new T[allocated_size];
    for (int i = 0; i < size; i++) {
      new_elements[i] = elements[i];
    }

    delete[] elements;
    elements = new_elements;
  }

  int random_pos() {
    // TODO(samuel) Move this to a library, I copy paste it all the time
    std::uniform_int_distribution<int> distribution(0, size - 1);
    return distribution(random_engine);
  }
};

// TODO(Samuel): move this to a library
bool get_seed(int *seed) {
  std::ifstream dev_random("/dev/random", std::ios::binary);
  dev_random.read(reinterpret_cast<char *>(seed), sizeof(int));

  if (dev_random.fail()) {
    std::cerr << "Failed to get random seed: " << strerror(errno) << "\n";
    return false;
  }

  return true;
}

int main() {

  int seed;
  if (!get_seed(&seed)) {
    return EXIT_FAILURE;
  }

  RandQ<std::string> randq(seed);

  randq.push("foo");
  randq.push("bar");
  randq.push("baz");
  randq.push("quux");
  randq.push("quuux");
  randq.push("quuuux");
  randq.push("quuuuux");
  randq.push("quuuuuux");
  randq.push("quuuuuuux");

  std::string out;

  while (randq.pop(&out)) {
    std::cout << out << "\n";
  }

  return 0;
}