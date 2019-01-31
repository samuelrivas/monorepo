// Copyright (C) 2019 by Samuel Rivas <samuelrivas@gmail.com>

/* This is a thread safe queue with blocking pop (i.e. it blocks when empty
   until push is called from another thread */
#ifndef __ASYNCQ_HPP__
#define __ASYNCQ_HPP__

#include <queue>
#include <mutex>
#include <condition_variable>

using std::queue;
using std::mutex;
using std::condition_variable;
using std::lock_guard;
using std::unique_lock;

namespace sam {
  template <typename T>
  class AsyncQueue {
    queue<T> q;
    mutex m;
    condition_variable cond;

  public:
    void push(T x) {
      {
        lock_guard<mutex> lg(m);
        q.push(x);
      }
      cond.notify_one();
    }

    T pop() {
      T result;
      {
        unique_lock<mutex> ul(m);
        while (q.size() == 0) {
          cond.wait(ul);
        }
        result = q.front();
        q.pop();
      }
      return result;
    }
  };
}

#endif
