#include <vector>
#include <cassert>
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
  vector<int> in  = { 5, 7, 2, 6, 1, 4, 3 };
  vector<int> pre = { 1, 2, 5, 7, 6, 3, 4 };

  const ProcessNode<int, void>* f = new PrintNode;

  TreeNode<int>* tree = reconstruct(in, pre);

  cout << "pre:";
  pre_order<int, void>(tree, f);
  cout << endl;

  cout << "in :";
  in_order<int, void>(tree, f);
  cout << endl;

  delete f;
  delete tree;

  return 0;
}