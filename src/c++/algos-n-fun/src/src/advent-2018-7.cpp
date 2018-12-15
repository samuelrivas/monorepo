#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <set>
#include <regex>

#include "lib/digraph.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::regex;
using std::regex_match;
using std::smatch;

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

int main(void) {

  cin.sync_with_stdio(false);
  set<char> nodes;

  // XXX Assume that we only have A - Z nodes
  Digraph graph('Z' - 'A' + 1);


  for (string line; getline(cin, line);) {
    pair<char, char> input = parse_input(line);
    nodes.insert(input.first);
    nodes.insert(input.second);

    // A node is connected to those that need to be completed directly before it
    graph.connect(node_to_id(input.second),
                  node_to_id(input.first));
  }

  vector<char> output;

  while (nodes.size() >  0) {
    for (char node : nodes) {
      bool ready = true;
      for (int dependency : graph.connected(node_to_id(node))) {

        if (nodes.find(id_to_node(dependency)) != nodes.end()) {
          cerr << node
               << " is blocked by "
               << id_to_node(dependency) << endl;
          ready = false;
          break;
        } else {
          cerr << node
               << " requires " << id_to_node(dependency)
               << " but is already done" << endl;
        }
      }

      if (ready) {
        cerr << "We can output " << node << endl;
        output.push_back(node);
        nodes.erase(node);
        break;
      }
    }
    cerr << nodes.size() << " to go!" << endl;
  }

  cout << "Solution: ";
  for (size_t i = output.size() - 1; i < output.size(); i--) {
    cout << output[i];
  }
  cout << endl;
  return 0;
}
