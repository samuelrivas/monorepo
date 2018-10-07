// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <unordered_map>
#include <string>
#include <utility>
#include <queue>
#include <set>
#include <iomanip>

using std::unordered_map;
using std::string;
using std::cout;
using std::endl;
using std::pair;
using std::queue;
using std::set;
using std::setw;

int main(void) {
  unordered_map<string, int> um {
    {"one", 1},
    {"two", 2}
  };

  cout << "unordered map" << endl;
  cout << "one:" << um["one"] << endl;
  cout << "one:" << um.at("one") << endl;
  cout << "three?" << (um.find("three") != um.end()) <<endl;
  cout << "three:" << um["three"] << endl;
  cout << "three?" << (um.find("three") != um.end()) <<endl;
  cout << "three:" << um.at("three") << endl; // this got inserted above
  

  for (pair<string, int> i : um) {
    cout << "(" << i.first << "," << i.second << ")";
  }
  cout << endl;

  cout << "queue" << endl;
  queue<int> q;
  q.push(1);
  cout << q.front() << endl;
  q.push(2);
  cout << q.front() << endl;
  q.pop();
  cout << q.front() << endl;
  q.push(3);
  cout << q.front() << endl;
  q.pop();
  cout << q.front() << endl;

  cout << "(ordered) set" << endl;
  set<int> s { 1, 1, 4, 0, 3, 4, 0, 2, 5, 5, 5, 1, 3, 5, 1, 5, 2, 0, 1, 0 };

  for (int i : s) {
    cout << setw(3) << i;
  }
  cout << endl;

  return 0;
}
