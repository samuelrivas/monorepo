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

void task(int task_id,
          default_random_engine random_engine,
          mutex* mq, // queue mutex
          mutex *mprint, // io mutex
          condition_variable* cond,
          queue<Result>* q) {

  bernoulli_distribution distribution;
  int count = 0;

  while (distribution(random_engine)) {
    sleep_for(seconds(1));
    count++;
  }

  {
    lock_guard<mutex> lg(*mq);
    q -> push({ task_id, count });
  }
  cond -> notify_one();
  {
    lock_guard<mutex> lg(*mprint);
    cerr << "Task " << task_id << " finished" << endl;
  }
}

int main() {
  queue<Result> results;
  mutex mq;
  mutex mprint;
  condition_variable cond;

  for (int i = 0; i < THREADS; i++) {
    int seed;
    bool success = get_seed(&seed);
    assert(success);

    default_random_engine random_engine { seed };

    thread(task, i, random_engine, &mq, &mprint, &cond, &results).detach();

    {
      lock_guard<mutex> lg(mprint);
      cerr << "Task " << i << " launched" << endl;
    }
  }

  int gotten = 0;
  int maxim = numeric_limits<int>::min();
  while (true) {
    unique_lock<mutex> lk(mq);
    while (results.size() > 0) {
      Result result;
      result = results.front();
      results.pop();

      gotten++;
      maxim = max(maxim, result.value);
      {
        lock_guard<mutex> lg(mprint);
        cerr << "Result from " << result.task_id << ": "
             << result.value << endl;
      }
    }
    if (gotten < THREADS) {
      cond.wait(lk);
    } else {
      break;
    }
  }
  {
    lock_guard<mutex> lg(mprint);
    cout << "Result: " << maxim << endl;
  }
  return 0;
}
