/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Create a tree from a heap(ish)
 */

#include <vector>
#include <iostream>
#include <iomanip>

#include "lib/binary-tree.hpp"

using std::vector;
using std::cout;
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
  // Using -1 as null here, will convert below
  vector<vector<int>> tests {
    {1, 2, 3, 4, 5, 6, 7, 8, 9, 10},  // To verify order easily
    {5, 4, 6, 3, -1, -1, 7},  // V-shaped bst
    {5, 3, 8, 2, 4, 6, -1, 1}  // Packed bst
  };
  const ProcessNode<int, void>* f = new PrintNode;

  for (auto test : tests) {
    vector<int*> test_p(test.size());

    for (size_t i = 0; i < test.size(); i++) {
      test_p[i] = test[i] != -1 ? &test[i] : nullptr;
    }

    TreeNode<int>* tree = from_heap(test_p);

    cout << "in order  :";
    in_order<int, void>(tree, f);
    cout << endl << "pre order :";
    pre_order<int, void>(tree, f);
    cout << endl << "post order:";
    post_order<int, void>(tree, f);
    cout << endl;
    cout << endl;

    delete tree;
  }
  delete f;
  return 0;
}
