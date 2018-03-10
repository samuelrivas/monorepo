#include <vector>
#include <cassert>
#include <iomanip>

#include "lib/binary-tree.hpp"

using std::vector;
using std::cout;
using std::endl;
using std::setw;

// start is included
template<typename T>
TreeNode<T>* reconstruct(const vector<T>& in_order, const vector<T>& pre_order,
                         size_t start_in, size_t start_pre, size_t size) {
  assert(in_order.size() == pre_order.size());

  if (size == 0) {
    return nullptr;
  }

  T root_val = pre_order[start_pre];
  TreeNode<T>* root = new TreeNode<T>(root_val);

  size_t elements_to_root = 0;
  for (size_t i = start_in;
       in_order[i] != root_val && i < start_in + size;
       i++, elements_to_root++) { };

  assert(elements_to_root < size);

  root -> left = reconstruct(in_order, pre_order,
                             start_in, start_pre + 1, elements_to_root);

  root -> right = reconstruct(in_order, pre_order,
                              start_in + elements_to_root + 1,
                              start_pre + elements_to_root + 1,
                              size - elements_to_root - 1);

  return root;
}

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

  TreeNode<int>* tree = reconstruct(in, pre, 0, 0, in.size());

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