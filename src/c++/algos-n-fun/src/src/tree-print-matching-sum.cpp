/* Copyright 2018 <samuelrivas@gmail.com */
#include <iostream>
#include <queue>
#include <iomanip>
#include <vector>

using std::queue;
using std::vector;
using std::cout;
using std::endl;
using std::setw;

#include "lib/binary-tree.hpp"

void print_queue(queue<int>* x) {
  while (!x -> empty()) {
    cout << setw(3) << x -> front();
    x -> pop();
  }
}

void print_all_paths(const TreeNode<int>* node, int target, queue<int> path,
                     int path_sum) {
  if (node == nullptr) {
    return;
  }

  while (!path.empty() && path_sum + node -> value > target) {
    int to_remove = path.front();
    path.pop();
    path_sum -= to_remove;
  }

  path.push(node-> value);
  path_sum += node -> value;

  if (path_sum == target) {
    cout << "path found:";
    queue<int> aux_q = path;
    print_queue(&aux_q);
    cout << endl;
  }

  print_all_paths(node -> left, target, path, path_sum);
  print_all_paths(node -> right, target, path, path_sum);
}

int main(void) {
  vector<int> in_order = { 7, 3, 1, 2, 0, 5, 4, 6 };
  vector<int> pre_order = {5, 3, 7, 2, 1, 0, 4, 6 };
  TreeNode<int> *tree = reconstruct(in_order, pre_order);

  cout << "5:" << endl;
  print_all_paths(tree, 5, queue<int>(), 0);

  cout << endl << "10:" << endl;
  print_all_paths(tree, 10, queue<int>(), 0);

  delete tree;
  return 0;
}
