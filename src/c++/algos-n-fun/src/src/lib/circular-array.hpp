#ifndef __CIRCULAR_ARRAY_HPP__
#define __CIRCULAR_ARRAY_HPP__

#include <vector>
#include <initializer_list>

using std::vector;

template <typename T>
class CircularArray {
private:
  vector<T> content;
  int pos = 0;
public:
  explicit CircularArray(std::initializer_list<T> _content) :
    content { _content } {
  }

  explicit CircularArray(const vector<T>& _content) :
    content { _content } {
  }

  void fw() {
    pos = modulo(pos + 1);
  }

  void bw() {
    pos = modulo(pos - 1);
  }

  void set_pos(int n) {
    pos = modulo(n);
  }

  void increment_pos(int n) {
    pos = modulo(pos + n);
  }

  T get() const {
    return content[pos];
  }

  T get_and_fw() {
    T value = content[pos];
    fw();
    return value;
  }

  T get_and_bw() {
    T value = content[pos];
    bw();
    return value;
  }

  T fw_and_get() {
    fw();
    return content[pos];
  }

  T bw_and_get() {
    bw();
    return content[pos];
  }
private:
  // In c++, -x % m yields -(x % m), which is a bit problematic if we use the
  // result for indexing arrays ;)
  //
  // Also, don't forget to cast size to int, or you'll end up running unsigned
  // modulo, for which -1 % 3 is 0 ...
  int modulo(int n) {
    int m = n % static_cast<int>(content.size());
    return m >= 0 ? m : content.size() + m;
  }
};

#endif
