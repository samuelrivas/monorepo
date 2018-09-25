// Copyright (C) 2018 by Samuel Rivas <samuelrivas@gmail.com>
#include <vector>
#include <utility>
#include <iostream>
#include <optional>
#include <cassert>
#include <algorithm>
#include <set>

using std::vector;
using std::pair;
using std::cout;
using std::cerr;
using std::endl;
using std::max;
using std::min;
using std::set;
using std::sort;

typedef enum class event_type {
  BIRTH,
  DEATH
} event_type;

class CompareEvents {
public:
  bool operator() (pair<int, event_type> a, pair<int, event_type>b) {
    return a.first < b.first;
  }
} compareEvents;

set<int> max_population_efficient(const vector<pair<int, int>>& years) {

  // Create a vector with pairs year-event-type
  vector<pair<int, event_type>> events(2*years.size());
  for (size_t i = 0; i < years.size(); i ++) {
    events[2*i] = { years[i].first, event_type::BIRTH };
    events[2*i + 1] = { years[i].second, event_type::DEATH };
  }

  sort(events.begin(), events.end(), compareEvents);

  int max_population = 0;
  int current_population = 0;
  set<int> max_years; // XXX we are not getting all max_years here

  for (pair<int, event_type> e : events) {
    if (e.second == event_type::BIRTH) {
      current_population ++;
    }
    cerr << e.first << ":" << current_population << endl;
    if (current_population == max_population) {
      max_years.insert(e.first);
    } else if (current_population > max_population) {
      max_population = current_population;
      max_years.clear();
      max_years.insert(e.first);
    }

    if (e.second == event_type::DEATH) {
      current_population --;
    }
  }
  return max_years;
}

pair<int, int> low_high_years(const vector<pair<int, int>>& years) {
  assert(years.size() > 0);

  int low = years[0].first;
  int high = years[0].first;

  for (pair<int, int> lifespan : years) {
    low = min<int>(lifespan.first, low);
    low = min<int>(lifespan.second, low);

    high = max<int>(lifespan.first, high);
    high = max<int>(lifespan.second, high);
  }
  return {low, high};
}

set<int> max_population(const vector<pair<int, int>>& years) {
  pair<int,int> extremes = low_high_years(years);
  vector<int> alive(extremes.second - extremes.first + 1, 0);

  for (pair<int,int> lifespan : years) {
    for (int i = lifespan.first; i <= lifespan.second; i++) {
      alive[i - extremes.first]++;
    }
  }

  int max_population = 0;
  set<int> max_years;

  for (size_t i = 0; i < alive.size(); i++) {
    if (alive[i] == max_population) {
      max_years.insert(extremes.first + i);
    } else if (alive[i] > max_population) {
      max_population = alive[i];
      max_years.clear();
      max_years.insert(extremes.first + i);
    }
  }
  return max_years;
}

int main(void) {
  vector<pair<int, int>> testcase = {
    {10, 11},
    {0, 4},
    {6, 8},
    {1, 6},
    {3, 6}
  };

  for (int y : max_population_efficient(testcase)) {
    cout << "max: " << y << endl;
  }
  return 0;
}
