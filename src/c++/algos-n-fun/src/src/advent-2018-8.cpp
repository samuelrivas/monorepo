#include <iostream>
#include <istream>
#include <string>
#include <cassert>
#include <vector>
#include <utility>
#include <iterator>

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::pair;
using std::istream_iterator;
using std::back_inserter;
using std::copy;

int sum_metadata(const vector<int>& tree, size_t* read_head)  {
  size_t children = tree.at((*read_head)++);
  size_t metadata_elements = tree.at((*read_head)++);
  int metadata_sum = 0;
  for (size_t child = 0; child < children; child++) {
    metadata_sum += sum_metadata(tree, read_head);
  }
  for (size_t i = 0; i < metadata_elements; i++) {
    metadata_sum += tree.at((*read_head)++);
  }
  return metadata_sum;
}

int tree_value(const vector<int>& tree, size_t* read_head)  {
  size_t root = *read_head;
  size_t children = tree.at((*read_head)++);
  size_t metadata_elements = tree.at((*read_head)++);

  vector<int> child_values(children);
  for (size_t child = 0; child < children; child++) {
    child_values.at(child) = tree_value(tree, read_head);
  }

  int value = 0;
  for (size_t i = 0; i < metadata_elements; i++) {
    int metadata = tree.at((*read_head)++);
    cerr << root << " reading metadata " << metadata << endl;
    if (children == 0) {
      cerr << root << " is a tree without children" << endl;
      value += metadata;
      cerr << "Value: " << value << endl;
    } else {
      value += (metadata - 1 < static_cast<int>(children))
        ? child_values.at(metadata - 1)
        : 0;
      cerr << root << " value after child " << metadata
           << ": " << value << endl;
    }
  }
  return value;
}

int main(void) {
  vector<int> input;
  istream_iterator<int> input_iter(cin);
  istream_iterator<int> eos;
  copy(input_iter, eos, back_inserter(input));
  size_t read_head = 0;

  cout << "Solution 1: " << sum_metadata(input, &read_head) << endl;

  read_head = 0;

  cout << "Solution 2: " << tree_value(input, &read_head) << endl;
  return 0;
}
