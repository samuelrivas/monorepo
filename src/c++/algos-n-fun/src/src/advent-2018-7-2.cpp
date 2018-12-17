#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <set>
#include <unordered_set>
#include <regex>
#include <queue>

#include "lib/digraph.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::unordered_set;
using std::regex;
using std::regex_match;
using std::smatch;
using std::priority_queue;

// finish time, task
typedef pair<int,char> Event;

struct EventPriority {
  bool operator() (Event a, Event b) {
    return a.first > b.first;
  }
};

pair<char,char> parse_input(string input) {
  regex line("^Step (.) must be finished before step (.) can begin.$");
  smatch matches;

  if (regex_match(input, matches, line)) {
    assert(matches.size() == 3);
    char first = matches[1].str()[0];
    char second = matches[2].str()[0];
    return { first, second };
  }
  assert(false);
}

int node_to_id(char node) {
  return node - 'A';
}

char id_to_node(int id) {
  return id + 'A';
}

char task_duration(char task) {
  return 61 + task - 'A';
}

int main(void) {

  cin.sync_with_stdio(false);
  set<char> pending;

  // XXX Assume that we only have A - Z nodes
  Digraph graph('Z' - 'A' + 1);

  for (string line; getline(cin, line);) {
    pair<char, char> input = parse_input(line);
    pending.insert(input.first);
    pending.insert(input.second);

    // A node is connected to those that need to be completed directly before it
    graph.connect(node_to_id(input.second),
                  node_to_id(input.first));
  }
  set<char> ready;
  unordered_set<char> done;
  vector<char> output;
  priority_queue<Event, vector<Event>, EventPriority> events;
  int wall_clock = 0;
  int available_workers = 5;

  while (!(ready.empty() && pending.empty())) {

    // Gather ready tasks
    for (char task : pending) {
      bool is_ready = true;
      for (int dependency : graph.connected(node_to_id(task))) {

        if (done.find(id_to_node(dependency)) == done.end()) {
          cerr << task
               << " is blocked by "
               << id_to_node(dependency) << endl;
          is_ready = false;
          break;
        } else {
          cerr << task
               << " requires " << id_to_node(dependency)
               << " but is already done" << endl;
        }
      }

      if (is_ready) {
        cerr << task << " can be done whenever now" << endl;
        ready.insert(task);
      }
    }

    // Somewhat lazy update, this will do a lot of repeated work
    for (char task : ready) {
      pending.erase(task);
    }

    // Schedule work to do;
    while (available_workers > 0 && ready.size() > 0) {
      char task = *(ready.begin());
      int duration = task_duration(task);

      cerr << wall_clock << ": Starting task " << task << " of duration " << duration << endl;
      ready.erase(task);
      available_workers--;
      events.push({wall_clock + duration, task});
    }

    cerr << "There are " << ready.size() << " tasks that could be done with more elves"
         << endl;

    Event next = events.top();
    events.pop();
    cerr << "Next event is the completion of " << next.second
         << " at "
         << next.first << endl;

    wall_clock = next.first;
    available_workers++;
    ready.erase(next.second);
    done.insert(next.second);
    output.push_back(next.second);
    cerr << pending.size() << " pending and " << ready.size() << " ready " << endl;
  }

  cout << "Solution: " << wall_clock << endl;
  cout << "Order: "
       << string(output.cbegin(), output.cend())
       << endl;
  return 0;
}
