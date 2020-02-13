#include <iostream>

#include "si_units.hpp"

using namespace si;

auto speed(Length x, Time t) { return x / t; }

int main() {
  watt_t<int> power{45};

  candela_t<int> energy{4};

  auto t = power / energy;

  // t = t * t;

  std::cout << t << ' ' << t.symbol() << std::endl;

  return 0;
}
