#include <random>
#include <iostream>
#include <cassert>
#include <chrono>
#include <thread>
#include <future>
#include <vector>

#include <rnd.hpp>

const int THREADS = 4;

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

void task(int task_id, default_random_engine random_engine, promise<int> result) {
  bernoulli_distribution distribution;
  int count = 0;

  while (distribution(random_engine)) {
    sleep_for(seconds(3));
    count++;
  }
  result.set_value(count);
  cerr << "task " << task_id << " finished" << endl;
}

int main() {
  vector<future<int>> results;

  for (int i = 0; i < THREADS; i++) {
    int seed;
    bool success = get_seed(&seed);
    assert(success);

    default_random_engine random_engine { seed };

    promise<int> prom;
    results.push_back(prom.get_future());

    thread(task, i, random_engine, move(prom)).detach();

    cerr << "Task " << i << " launched" << endl;
  }

  for (int i = 0; i < THREADS; i++) {
    cout << "Output: " << results[i].get() << endl;
  }

  return 0;
}
