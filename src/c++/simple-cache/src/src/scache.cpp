/* Copyright 2020 samuelrivas@gmail.com
 *
 * Simple, expiring cache
 */

#include <time.h>

#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <utility>

using std::cout;
using std::cerr;
using std::endl;
using std::unordered_map;
using std::priority_queue;
using std::pair;
using std::string;

namespace sam {

template <typename K, typename V>
class SCache {
  using Age = int;
  using Queue_frame = pair<Age, K>;

  // Key to Value, age map. We keep the age so that we can know if a value is
  // expired, but not flushed
  unordered_map<K, pair<V, Age>> cache;

  // Reversed priority queue with <age, key> pairs, the tip is the lowest age
  // value, and its associated key
  priority_queue<
    Queue_frame,
    std::vector<Queue_frame>,
    std::greater<Queue_frame>
    > ages;

 public:
  SCache() { }

  // TODO(Samuel) flush when content has reached age
  void insert(const K k, const V v, Age age) {
    cache[k] = { v, age };
    ages.push({ age, k });
  }

  // Throws if k does not exist
  const pair<V, K>& lookup(K k) const {
    return cache.at(k);
  }

  // Deletes all entries younger than age
  void flush(Age age) {
    while (ages.size() > 0 && ages.top().first < age) {
      const pair<Age, K>& young = ages.top();

      cerr << "Youngest is " << young.second << " at " << young.first << endl;

      const K& k = young.second;

      // K is *potentially* old. We cannot know for sure since it could've been
      // modified at later time. Hence we need to check again before deleting it
      typename unordered_map<K, pair<V, Age>>::iterator it = cache.find(k);
      if (it != cache.end() && (it -> second).second < age) {
        cache.erase(it);
        cerr << "deleted" << endl;
      } else {
        cerr << "this key isn't there, or is younger now" << endl;
      }
      ages.pop();
    }
  }
};
}  // namespace sam

using sam::SCache;

int main() {
  SCache<int, string> cache;

  cache.insert(10, "foo", 0);
  cache.insert(20, "bar", 3);
  cache.insert(10, "fooo", 4);
  cache.insert(30, "kuux", 2);

  // cache.insert(10, 0, 0);
  // cache.insert(20, 1, 0);
  // cache.insert(10, 2, 0);
  // cache.insert(30, 3, 0);

  cout << cache.lookup(10).first << endl;
  cout << cache.lookup(20).first << endl;
  cout << cache.lookup(30).first << endl;

  cout << "flushing 3" << endl;
  cache.flush(3);
  cout << "flushing 10" << endl;
  cache.flush(10);
  return 0;
}
