#include <rapidcheck.h>

#include <vector>
#include <algorithm>

int main() {
  rc::check("double reversal yields the original value",
            [](const std::vector<int> &l0) {
              auto l1 = l0;
              std::reverse(begin(l1), end(l1));
              std::reverse(begin(l1), end(l1));
              if (l0.size() == 4) {
                l1[2] = 0;
              }
              RC_ASSERT(l0 == l1);
            });

  return 0;
}
