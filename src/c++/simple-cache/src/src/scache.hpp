/* Copyright 2020 samuelrivas@gmail.com
 *
 * Simple, expiring cache.
 *
 * You add a key value pair with the insert function. When you insert a key
 * value pair, you need to provide a timestamp. Higher values for age
 */
#ifndef __SCACHE_HPP__
#define __SCACHE_HPP__

#include <queue>
#include <string>
#include <unordered_map>
#include <utility>

using std::cerr;
using std::endl;
using std::unordered_map;
using std::priority_queue;
using std::pair;

namespace sam {

template <typename K, typename V>
class SCache {
  using Timestamp = int;
  using Queue_frame = pair<Timestamp, K>;

  // Key to (Value, timestamp) map.
  unordered_map<K, pair<V, Timestamp>> cache;

  // Reversed priority queue with <timestamp, key> pairs, the tip is the lowest
  // timestamp value, and its associated key. Note that entries in this queue
  // are never modified, so they are potentially stale, cache is the source of
  // truth.
  priority_queue<
    Queue_frame,
    std::vector<Queue_frame>,
    std::greater<Queue_frame>
    > timestamps;

 public:
  SCache() { }

  void insert(const K k, const V v, Timestamp timestamp) {
    cache[k] = { v, timestamp };
    timestamps.push({ timestamp, k });
  }

  // Throws if k does not exist
  // TODO(samuel) provide a better interface for this
  const pair<V, K>& lookup(K k) const {
    return cache.at(k);
  }

  // Deletes all entries with a timestamp earlier than the one provided in the
  // arguments
  void flush(Timestamp timestamp) {
    while (timestamps.size() > 0 && timestamps.top().first < timestamp) {
      const pair<Timestamp, K>& young = timestamps.top();

      cerr << "Youngest is " << young.second << " at " << young.first << endl;

      const K& k = young.second;

      // K is *potentially* old. We cannot know for sure since it could've been
      // modified at later time. Hence we need to check again before deleting it
      typename unordered_map<K, pair<V, Timestamp>>::iterator it = cache.find(k);
      if (it != cache.end() && (it -> second).second < timestamp) {
        cache.erase(it);
        cerr << "deleted" << endl;
      } else {
        cerr << "this key isn't there, or is younger now" << endl;
      }
      timestamps.pop();
    }
  }
};
}  // namespace sam

#endif
