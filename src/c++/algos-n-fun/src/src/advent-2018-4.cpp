#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <unordered_map>
#include <cassert>
#include <exception>
#include <sstream>
#include <cassert>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::getline;
using std::string;
using std::vector;
using std::pair;
using std::regex;
using std::regex_match;
using std::smatch;
using std::ssub_match;
using std::sregex_iterator;
using std::unordered_map;
using std::terminate;
using std::istringstream;

typedef vector<pair<int,int>> Naps;
typedef unordered_map<int, Naps> Guard_log;

enum class Event_type {
  Change,
  Sleep,
  Wake
};

typedef struct Event {
  Event_type type;
  union Info {
    int minute;
    int guard;
  } info;
} Event;

// Surely, there must be a better way of doing this
int match_to_int(const string& match) {
  istringstream istr(match);
  int out;
  istr >> out;
  return out;
}

Event parse_line(string line) {
  regex log_line(".*([[:digit:]]{2})\\] (.*)$");
  smatch matches;

  if (!regex_match(line, matches, log_line)) {
    cerr << "This line kinda doesn't match:" << line << endl;
    terminate();
  }

  int minute = match_to_int(matches[1]);
  string event_line = matches[2];

  Event event;
  event.info.minute = minute;

  if (event_line == "falls asleep") {
    event.type = Event_type::Sleep;
  } else if (event_line == "wakes up") {
    event.type = Event_type::Wake;
  } else {
    event.type = Event_type::Change;

    regex guard("Guard #([[:digit:]]+) .*");
    if (!regex_match(event_line, matches, guard)) {
      cerr << "This thing kinda doesn't match: " << event_line << endl;
      terminate();
    }
    event.info.guard = match_to_int(matches[1]);
  }
  return event;
}

pair<int, int> find_laziest_minute(Naps naps) {
  // Lazy search since this is fast:
  vector<int> minute_counts(100);
  int max_count = 0;
  int max_minute = -1;

  for (pair<int, int> nap : naps) {
    for (int i = nap.first; i < nap.second; i++) {
      minute_counts[i]++;
      if (minute_counts[i] >= max_count) {
        max_minute = i;
        max_count = minute_counts[i];
        cerr << "The laziest minute is " << max_minute
             << " with " << max_count << " naps" << endl;
      }
    }
  }
  return {max_minute, max_count};
}

pair<int, int> find_laziest_guard_and_minute(Guard_log log) {
  int laziest_guard = -1;
  int laziest_minute = -1;
  int max_naps = -1;

  for (pair<int, Naps> guard_data : log) {
    pair<int, int> laziest = find_laziest_minute(guard_data.second);
    if (laziest.second > max_naps) {
      laziest_guard = guard_data.first;
      laziest_minute = laziest.first;
      max_naps = laziest.second;

      cerr << "Guard " << laziest_guard
           << " set a new record in " << laziest_minute
           << " with a stunning " << max_naps << " naps" << endl;

    }
  }
  return {laziest_guard, laziest_minute};
}

int main(void) {
  // Read the input in our guards structure, and find the laziest guard while we
  // are at it
  unordered_map<int, int> sleeping_times;
  int max_sleeping_time = 0;
  int laziest_guard = -1;
  Guard_log log;

  int current_guard = -1;
  int nap_start_time = -1;

  for (string line; getline(cin, line);) {
    Event event = parse_line(line);

    switch (event.type) {
    case Event_type::Change:
      current_guard = event.info.guard;
      // cerr << "Changing to guard " << current_guard << endl;
      break;
    case Event_type::Sleep:
      assert(current_guard != -1);
      assert(nap_start_time == -1);

      // cerr << "Guard " << current_guard << " sleeps " <<
      //   event.info.minute << endl;

      nap_start_time = event.info.minute;
      break;
    case Event_type::Wake:
      assert(current_guard != -1);
      assert(nap_start_time != -1);

      // cerr << "Guard " << current_guard << " wakes " <<
      //   event.info.minute << endl;

      Naps naps = log[current_guard];
      int acc_sleep = sleeping_times[current_guard];
      sleeping_times[current_guard] =
        acc_sleep + event.info.minute - nap_start_time;

      if (sleeping_times[current_guard] > max_sleeping_time) {
        max_sleeping_time = sleeping_times[current_guard];
        laziest_guard = current_guard;
        cerr << "And the laziest guard goes to " << current_guard
             << " with " << max_sleeping_time << " minutes!" << endl;
      };

      naps.push_back({nap_start_time, event.info.minute});
      log[current_guard] = naps;

      nap_start_time = -1;
      break;
    }
  }

  cout << "Laziest guard: " << laziest_guard << endl;

  // Get the laziest minute of the laziest guard
  pair<int, int> laziest_minute = find_laziest_minute(log[laziest_guard]);

  cout << "Laziest minute: " << laziest_minute.first << endl;
  cout << "That is: " << laziest_guard * laziest_minute.first << endl;

  // And we gan do part two as well here
  pair<int, int> laziest_guard_minute = find_laziest_guard_and_minute(log);
  cout << "Laziest guard in a single minute: "
       << laziest_guard_minute.first << endl;
  cout << "Laziest minute for a single guard: "
       << laziest_guard_minute.second << endl;
  cout << "That is: " << laziest_guard_minute.first * laziest_guard_minute.second << endl;

  return 0;
}
