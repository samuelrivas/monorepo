#include <random>
#include <iostream>
#include <cassert>
#include <chrono>
#include <thread>
#include <future>
#include <vector>
#include <mutex>
#include <queue>
#include <condition_variable>
#include <limits>
#include <algorithm>

#include <rnd.hpp>

using std::default_random_engine;
using sam::get_seed;
using std::bernoulli_distribution;
using std::cout;
using std::cerr;
using std::endl;
using std::chrono::seconds;
using std::this_thread::sleep_for;
using std::promise;
using std::future;
using std::vector;
using std::thread;
using std::move;
using std::queue;
using std::mutex;
using std::condition_variable;
using std::lock_guard;
using std::unique_lock;
using std::numeric_limits;
using std::max;

const int THREADS = 10;

struct Result {
  int task_id;
  int value;
};

template <typename T>
class AsyncQueue {
  queue<T> q;
  mutex m;
  condition_variable cond;

public:
  explicit AsyncQueue() { };

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

void task(int task_id,
          default_random_engine random_engine,
          mutex *mprint, // io mutex
          AsyncQueue<Result>* q) {

  bernoulli_distribution distribution;
  int count = 0;

  while (distribution(random_engine)) {
    sleep_for(seconds(1));
    count++;
  }
  q -> push({ task_id, count });

  {
    lock_guard<mutex> lg(*mprint);
    cerr << "Task " << task_id << " finished" << endl;
  }
}

int main() {
  AsyncQueue<Result> results;
  mutex mprint;

  for (int i = 0; i < THREADS; i++) {
    int seed;
    bool success = get_seed(&seed);
    assert(success);

    default_random_engine random_engine { seed };

    thread(task, i, random_engine, &mprint, &results).detach();

    {
      lock_guard<mutex> lg(mprint);
      cerr << "Task " << i << " launched" << endl;
    }
  }

  int maxim = numeric_limits<int>::min();
  for (int i = 0; i < THREADS; i++) {
    Result result = results.pop();
    maxim = max(maxim, result.value);

    {
      lock_guard<mutex> lg(mprint);
      cerr << "Result from " << result.task_id << ": "
           << result.value << endl;
    }
  }
  {
    lock_guard<mutex> lg(mprint);
    cout << "Result: " << maxim << endl;
  }
  return 0;
}
