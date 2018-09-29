// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>

#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <stack>

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::tuple;
using std::get;
using std::min;
using std::stack;
using std::pair;

int find_min(const vector<int>& histog, int start, int end) {
  int min_v = histog[start];;
  for (int i = start; i <= end; i ++) {
    min_v = min(histog[i], min_v);
  }
  return min_v;
}

void print_largest_rectangle_2(const vector<int>& histog) {
  stack<pair<int, int>> open_rectangles; // (x, y)
  tuple<int, int, int> best_rectangle; // (x1, x2, y)
  int best_area = 0;

  cerr << "New" << endl;

  for (size_t i = 0; i < histog.size(); i++) {

    // First close all the rectangles that are higher than the current height
    int opening_x = i; // Where will we open the new rectangle
    while (open_rectangles.size() > 0
           && open_rectangles.top().second > histog[i]) {
      pair<int, int> open_rectangle = open_rectangles.top();
      int area = (i - open_rectangle.first) * open_rectangle.second;

      cerr << "Closing ("
           << open_rectangle.first
           << ","
           << i - 1
           << ","
           << open_rectangle.second
           << ") -> "
           << area
           << endl;

      if (area > best_area) {
        best_area = area;
        best_rectangle = { open_rectangle.first, i, open_rectangle.second };
        cerr << "This was best" << endl;
      }

      opening_x = open_rectangle.first;
      open_rectangles.pop();
    }

    // open a new rectangle if needed
    assert(open_rectangles.size() == 0
           || open_rectangles.top().second <= histog[i]);

    if (open_rectangles.size() == 0
        || open_rectangles.top().second < histog[i]) {
      if (histog[i] > 0) {
        open_rectangles.push({ opening_x, histog[i]});
        cerr << "Opening (" << opening_x << "," << histog[i] << ")" << endl;
      }
    }
  }
  cout << "x1: "
       << get<0>(best_rectangle)
       << " x2: "
       << get<1>(best_rectangle) - 1
       << " y: "
       << get<2>(best_rectangle)
       << endl;
}

void print_largest_rectangle(const vector<int>& histog) {
  int max_area = 0;
  tuple<int, int, int> rectangle; // x1, x2, y

  for (size_t i = 0; i < histog.size(); i++) {
    for (size_t j = i; j < histog.size(); j++) {
      int y = find_min(histog, i, j);
      int area = (j - i + 1) * y;
      if (area > max_area) {
        max_area = area;
        rectangle = {i, j, y};
      }
    }
  }

  cout << "x1: "
       << get<0>(rectangle)
       << " x2: "
       << get<1>(rectangle)
       << " y: "
       << get<2>(rectangle)
       << endl;
}

int main() {
  // We finish with 0 as a neat trick to avoid a special recount at the end
  vector<vector<int>> tests = {
    {3, 4, 7, 2, 5, 1, 0}, // 10
    {1, 2, 3, 2, 1, 0}, // 6
    {1, 4, 4, 4, 1, 1, 1, 1, 1, 0}, // 12
    {1, 5, 3, 3, 1, 4, 5, 3, 4, 1, 0} // 12
  };

  for (auto test : tests) {
    print_largest_rectangle(test);
  }

  cout << "Now, faster" << endl;

  for (auto test : tests) {
    print_largest_rectangle_2(test);
  }
}
