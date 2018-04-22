#include <vector>
#include <iostream>
#include <iomanip>
#include <string>

using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::setw;
using std::string;

class UnionFind {
  vector<int> parent;
  vector<int> size;

  // TODO: flatten
  int find(int element) {
    if (parent[element] == element) {
      return element;
    } else {
      int new_root = find(parent[element]);
      if (new_root != parent[element]) {
        cerr << "flattening " << element << " to " << new_root << endl;
        size[parent[element]] -= size[element];
        parent[element] = new_root;
      }
      return new_root;
    }
  }

  void print_state() {
    cerr << "     ";
    for (size_t i= 0; i < size.size(); i++) {
      cerr << setw(3) << i;
    }
    cerr << endl;
    cerr << "root:";
    for (int x : parent) {
      cerr << setw(3) << x;
    }
    cerr << endl;
    cerr << "size:";
    for (int x : size) {
      cerr << setw(3) << x;
    }
    cerr << endl;
  }

 public:
  explicit UnionFind(int elements) : parent(elements), size(elements, 1) {
    for (int i = 0; i < elements; i++) {
      parent[i] = i;
    }
  }

  // C++ won't let us call this "union"
  void join(int x, int y) {
    cerr << "join(" << x << "," << y << ")" << endl;
    print_state();
    int root_x = find(x);
    int root_y = find(y);

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
    cerr << "after join" << endl;
    print_state();
  }

  bool joint(int x, int y) {
    return find(x) == find(y);
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
      string mark = uf.joint(x, y) ? " X" : "  ";
      cout << mark;
    }
    cout << endl;
  }
  cout << " ";
  for (int y = 0; y < 10; y++) {
    cout << " " << y;
  }
  cout << endl;

  uf.join(1, 6);
  return 0;
}
