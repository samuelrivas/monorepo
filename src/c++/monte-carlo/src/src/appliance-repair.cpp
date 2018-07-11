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
using boost::format;

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
        cerr << format("Coudln't generate an age larger than %f\n")
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

float fv(float pv, float periods, float rate) {
  return pv * pow(1 + rate, periods);
}

float pv(float fv, float periods, float rate) {
  return fv * pow(1 + rate, -periods);
}

// Future params

constexpr float appliance_life_mean = 10;
constexpr float appliance_life_sd = 2;
constexpr float appliance_current_life = 10;
constexpr float technician_assessment_cost = 1300;
constexpr float appliance_cost = 6000;
constexpr float inflation = 0.04;
constexpr float accounted_years = 30;
constexpr float reparation_cost = 1000;
constexpr int mc_runs = 100000;
constexpr float considered_failure = 1;

float calculate_cost(Appliance_simulator* simulator, bool repair,
                     int runs_to_show) {
  float mc_acc_cost = 0;

  for (int i = 0; i < mc_runs; i++) {
    float year = 0;
    float final_cost = 0;
    format payment_fmt("Payment at %0.2f: %d\n");

    if (repair) {

      float ttl = simulator -> get_conditional_lifetime(appliance_current_life)
        - appliance_current_life;

      if (i < runs_to_show) {
        cout << format("We get %0.2f years to live\n") % ttl;
        cout << payment_fmt % 0 % technician_assessment_cost;
      }

      final_cost = technician_assessment_cost;

      if (ttl > considered_failure) {
        if (i < runs_to_show) {
          cout << payment_fmt % 0 % reparation_cost;
        }
        final_cost += reparation_cost;
        year = ttl;
      }
    }

    do {
      float cost = pv(appliance_cost, year, inflation);
      final_cost += cost;

      if (i < 3) {
        cout << payment_fmt % year % cost;
      }

      float lifetime = simulator -> get_lifetime();
      year += lifetime;
    } while (year < accounted_years);

    if (i < 3) {
      cout << format("final_cost: %.2f\n") % final_cost;
    }
    mc_acc_cost += final_cost;
  }
  return mc_acc_cost / mc_runs;
}

int main() {

  int seed;
  if (!get_seed(&seed)) {
    cerr << "Cannot acquire random seed" << endl;
    exit(EXIT_FAILURE);
  }
  Appliance_simulator simulator(seed, appliance_life_mean,
                                appliance_life_sd);



  float repair_cost = calculate_cost(&simulator, true, 3);
  cout << format("Average cost with repair: %.2f\n") % repair_cost;

  float trash_cost = calculate_cost(&simulator, false, 3);
  cout << format("Average cost without repair: %.2f\n") % (trash_cost);

  cout << format("Repair - trash: %.2f\n") % (repair_cost - trash_cost);

  return 0;
}
