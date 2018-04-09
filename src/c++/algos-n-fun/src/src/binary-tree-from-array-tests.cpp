/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Create a tree from an array
 */

#include <vector>
#include <iostream>
#include <iomanip>

#include "lib/binary-tree.hpp"

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::setw;

class PrintNode : public ProcessNode<int, void> {
 public:
  PrintNode() { }
  virtual void operator() (TreeNode<int>* node) const {
    cout << setw(3) << node -> value;
  }
};

int main(void) {
  vector<int> nodes {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  TreeNode<int>* tree = from_vector(nodes);
  const ProcessNode<int, void>* f = new PrintNode;

  cout << "in order  :";
  in_order<int, void>(tree, f);
  cout << endl << "pre order :";
  pre_order<int, void>(tree, f);
  cout << endl << "post order:";
  post_order<int, void>(tree, f);
  cout << endl;

  delete tree;
  delete f;
  return 0;
}
