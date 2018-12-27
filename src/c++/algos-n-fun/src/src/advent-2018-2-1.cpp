#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <utility>

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::getline;
using std::vector;
using std::unordered_map;
using std::pair;

pair<int, int> get_score(const string& line) {
  unordered_map<char, int> counts;

  for (char x : line) {
    int count = counts[x];
    counts[x] = count + 1;
  }

  int twos = 0;
  int threes = 0;
  for (pair<char, int> x : counts) {
    if (x.second == 2) {
      twos = 1;
    }
    if (x.second == 3) {
      threes = 1;
    }
  }
  cout << twos << " " << threes << endl;
  return {twos, threes};
}

int main() {

  vector<string> ids;
  cin.sync_with_stdio(false);

  int twos = 0;
  int threes = 0;

  for (string line; getline(cin, line);) {
    pair<int, int> score = get_score(line);
    twos += score.first;
    threes += score.second;
  }

  cout  << twos << "*" << threes << "= " << twos * threes << endl;
  return 0;
}
