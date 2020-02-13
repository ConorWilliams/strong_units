#include <iostream>

#include "si_units.hpp"

auto speed(Length x, Time t) { return x / t; }

int main() {
    Length x = 9 * meter;
    Time t = 5 * second;

    using velocity = decltype(meter / second);

    velocity v = speed(x, t);

    std::cout << v << ' ' << v.symbol() << std::endl;

    return 0;
}