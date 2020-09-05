/* Copyright 2020 samuelrivas@gmail.com
 *
 * Thread safe wrapper for scache. Many readers are allowed to access the cache
 * Simultaneously, but at most one writer (and no reader) can access it at any
 * point in time.
 *
 * I am intentionally not making this a subclass of SCache, to avoid the burden
 * to make SCache extendable.
 */
#ifndef __SHARED_CACHE_HPP__
#define __SHARED_CACHE_HPP__

#include <shared_mutex>
#include <mutex>

#include "scache.hpp"

using std::shared_mutex;
using std::shared_lock;
using std::unique_lock;

namespace sam {
template <typename K, typename V>
class SharedCache {
  SCache<K, V> cache;
  shared_mutex mutex;

  using Timestamp = typename SCache<K, V>::Timestamp;

 public:
  SharedCache() { };

  void insert(const K k, const V v, Timestamp timestamp) {
    unique_lock lock(mutex);
    cache.insert(k, v, timestamp);
  }

  // Throws if k does not exist
  // TODO(samuel) provide a better interface for this
  const pair<V, Timestamp>& lookup(const K& k) {
    shared_lock lock(mutex);
    return cache.lookup(k);
  }

  // Deletes all entries with a timestamp earlier than the one provided in the
  // arguments
  void flush(Timestamp timestamp) {
    unique_lock lock(mutex);
    cache.flush(timestamp);
  }

  //Inserts and flushes in a single lock, for efficiency
  void insert_and_flush(const K k, const V v,
                        Timestamp insert_timestamp,
                        Timestamp flush_timestamp) {
    unique_lock lock(mutex);
    cache.insert(k, v, insert_timestamp);
    cache.flush(flush_timestamp);
  }
};
}  // namespace sam

#endif  // __SHARED_CACHE_HPP__
