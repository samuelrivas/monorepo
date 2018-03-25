// Copyright (C) 2018 by <samuelrivas@gmail.com>

/* unsorted_sets perform likely better, but I am too lazy to write a hash
   function. Also this is exponential, so we won't be running it for large sets
   anyway.

   Trying to be efficient with copies, so there are a bunch of tricky moves,
   beware if you start moving code around */
#include <set>
#include <iostream>
#include <string>
#include <utility>

using std::set;
using std::cout;
using std::endl;
using std::string;

template<typename T>
set<set<T>> all_subsets(set<T> x) {
  if (x.size() == 0) {
    set<set<T>> result;
    result.insert(move(x));
    return result;
  }

  T element = *(x.cbegin());

  set<T> without(move(x));
  without.erase(element);

  set<set<T>> remaining = all_subsets(move(without));

  set<set<T>> result(remaining);
  for (set<T> candidate : remaining) {
    candidate.insert(element);
    result.insert(move(candidate));
  }
  return result;
}

void print_sets(const set<set<int>>& sets) {
  for (set<int> xs : sets) {
    cout << "{";
    string sep = "";
    for (int x : xs) {
      cout << sep << x;
      sep = ",";
    }
    cout << "}" << endl;
  }
}

int main(void) {
  set<int> test = { 1, 2, 3 };
  print_sets(all_subsets<int>(test));
  return 0;
}
