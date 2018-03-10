/* Copyright 2018 <samuelrivas@gmail.com
 *
 * Homegronw binary trees
 */
#ifndef _BINARY_TREE_H_
#define _BINARY_TREE_H_

#include <vector>
#include <cstdlib>
#include <iostream>


using std::vector;

template<typename T>
class TreeNode {
 public:
  T value;
  TreeNode* left = nullptr;
  TreeNode* right = nullptr;

  explicit TreeNode(T x): value { x } { };
  ~TreeNode() {
    if (left) {
      delete left;
    }
    if (right) {
      delete right;
    }
  }
};

/* Visitors */

/* TODO: we probably want to pass an accumulator, but this is good for now */
template<typename T, typename R>
class ProcessNode {
public:
  ProcessNode() { };
  virtual ~ProcessNode() { };
  virtual R operator()(TreeNode<T>*) const = 0;
};



/* Constructs a tree with elements in positions i to j, all included. The layout
   of the resulting tree is such as a pre-order traversal yields the same order
   as the one in the array, and the tree has minimal height */
/* Builders */
namespace internal_builder {
  template<typename T>
  TreeNode<T>* from_vector(vector<T> elements, int i, int j) {
    if (i > j) {
      return nullptr;
    }

    TreeNode<int>* out = new TreeNode<T>(elements[i]);
    out -> left = from_vector(elements, i + 1, i + (j - i)/2);
    out -> right = from_vector(elements, i + (j - i)/2 + 1, j);
    return out;
  }

  template <typename T>
  TreeNode<T>* from_heap(const vector<T*>& elements, size_t pos) {
    if (pos >= elements.size()) {
      return nullptr;
    }

    if (elements[pos] == nullptr) {
      return nullptr;
    }

    TreeNode<T>* node = new TreeNode<T> (*(elements[pos]));
    node -> left = from_heap(elements, (pos + 1) * 2 - 1);
    node -> right = from_heap(elements, (pos + 1) * 2);

    return node;
  }
}

template<typename T>
TreeNode<T>* from_vector(vector<T> elements) {
  return internal_builder::from_vector(elements, 0, elements.size() - 1);
}

template <typename T>
TreeNode<T>* from_heap(const vector<T*>& elements) {
  return internal_builder::from_heap(elements, 0);
}

/* Traversals */

template<typename T, typename R>
void in_order(TreeNode<T>* tree, const ProcessNode<T, R>* f) {
  if (!tree) {
    return;
  }
  in_order(tree -> left, f);
  (*f)(tree);
  in_order(tree -> right, f);
}

template<typename T, typename R>
void pre_order(TreeNode<T>* tree, const ProcessNode<T, R>* f) {
  if (!tree) {
    return;
  }
  (*f)(tree);
  pre_order(tree -> left, f);
  pre_order(tree -> right, f);
}

template<typename T, typename R>
void post_order(TreeNode<T>* tree, const ProcessNode<T, R>* f) {
  if (!tree) {
    return;
  }
  post_order(tree -> left, f);
  post_order(tree -> right, f);
  (*f)(tree);
}

#endif
