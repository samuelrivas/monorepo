//Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>
#include <cstdio>
#include <boost/format.hpp>
#include <random>
#include <fstream>
#include <cstring>
#include <cmath>
#include <cassert>

using std::cout;
using std::cerr;
using std::endl;

std::lognormal_distribution<float> lognormal(float mean, float sd) {
  float log_mean = log(mean) - 1/2.0 * log (pow(sd/mean, 2) + 1);
  float log_sd = sqrt(log(pow(sd/mean, 2) + 1));

  return std::lognormal_distribution<float>(log_mean, log_sd);
}

class Appliance_simulator {
  std::default_random_engine random_engine;
  std::lognormal_distribution<float> lifetime_distribution;

 public:

  explicit Appliance_simulator(int seed, float mean_lifetime, float sd) :
    random_engine {
        static_cast<std::default_random_engine::result_type>(seed)
          },
    lifetime_distribution(lognormal(mean_lifetime, sd))
  { }

  float get_lifetime() {
    return lifetime_distribution(random_engine);
  }

  float get_conditional_lifetime(int current_age) {
    assert(current_age >= 0);

    constexpr int MAX_ATTEMPTS = 10000;
    int count = 0;

    while (true) {
      float lifetime = get_lifetime();
      count ++;

      if (lifetime > current_age) {
        return lifetime;
      }

      if (count > MAX_ATTEMPTS) {
        cerr << boost::format("Coudln't generate an age larger than %f\n")
          % current_age;
        exit(EXIT_FAILURE);
      }
    }
  }
};

bool get_seed(int *seed) {
  std::ifstream dev_random("/dev/random", std::ios::binary);
  dev_random.read(reinterpret_cast<char *>(seed), sizeof(int));

  if (dev_random.fail()) {
    cerr << "Failed to get random seed: " << strerror(errno) << "\n";
    return false;
  }

  return true;
}

int main() {
  int seed;
  if (!get_seed(&seed)) {
    cerr << "Cannot acquire random seed" << endl;
    exit(EXIT_FAILURE);
  }

  std::default_random_engine random_engine {
    static_cast<std::default_random_engine::result_type>(seed)
      };

  constexpr float mean = 10;
  constexpr float sd = 4;

  Appliance_simulator simulator(seed, mean, sd);

  float se = 0;
  float acc = 0;
  float count = 0;

  for (int i = 0; i < 25; i++, count++) {
    float life =  simulator.get_lifetime();
    acc += life;
    se += pow(life - mean, 2);
    cout << boost::format("Life: %.2f\n") % life;
  }

  cerr << boost::format("mean    : %.4f\n") % (acc / count);
  cerr << boost::format("variance: %.4f\n") % (se / count);
  cerr << boost::format("SD      : %.4f\n") % sqrt(se/ count);

  cerr << boost::format("Ha! %.4f\n") % simulator.get_conditional_lifetime(15);
  cerr << boost::format("Ha! %.4f\n") % simulator.get_conditional_lifetime(15);
  cerr << boost::format("Ha! %.4f\n") % simulator.get_conditional_lifetime(15);
  return 0;
}
