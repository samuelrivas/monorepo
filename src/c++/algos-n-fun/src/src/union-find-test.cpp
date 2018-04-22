#include <vector>
#include <iostream>
#include <iomanip>
#include <string>

using std::vector;
using std::cout;
using std::endl;
using std::setw;
using std::string;

class UnionFind {
  vector<int> parent;
  vector<int> size;

  // TODO: flatten
  int root(int element) {
    int root;
    for (root = parent[element]; parent[root] != root; root = parent[root]) { }
    return root;
  }

  void print_state() {
    cout << "     "; 
    for (size_t i= 0; i < size.size(); i++) {
      cout << setw(3) << i;
    }
    cout << endl;
    cout << "root:";
    for (int x : parent) {
      cout << setw(3) << x;
    }
    cout << endl;
    cout << "size:";
    for (int x : size) {
      cout << setw(3) << x;
    }
    cout << endl;
  }

 public:
  explicit UnionFind(int elements) : parent(elements), size(elements, 1) {
    for (int i = 0; i < elements; i++) {
      parent[i] = i;
    }
  }

  void join(int x, int y) {
    cout << "join(" << x << "," << y << ")" << endl;
    print_state();
    int root_x = root(x);
    int root_y = root(y);

    if (root_x == root_y) {
      return;
    }
    int size_root_x = size[root_x];
    int size_root_y = size[root_y];

    if (size_root_x > size_root_y) {
      parent[root_y] = root_x;
      size[root_x] += size_root_y;
    } else {
      parent[root_x] = root_y;
      size[root_y] += size_root_x;
    }
    cout << "after join" << endl;
    print_state();
  }

  bool find(int x, int y) {
    return root(x) == root(y);
  }
};

int main(void) {

  UnionFind uf(10);

  uf.join(1, 2);
  uf.join(1, 3);
  uf.join(3, 2);
  uf.join(5, 6);
  uf.join(1, 6);

  for (int x = 9; x >= 0; x--) {
    cout << x;
    for (int y = 0; y < 10; y++) {
      string mark = uf.find(x, y) ? " X" : "  ";
      cout << mark;
    }
    cout << endl;
  }
  cout << " ";
  for (int y = 0; y < 10; y++) {
    cout << " " << y;
  }
  cout << endl;

  return 0;
}
