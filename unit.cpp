#include <iostream>

#include "unit2.hpp"

using kelvin = unit<double, scale<>, si::meter<>, si::second<>>;
using degree = unit<double, scale<10, 99>, si::meter<>, si::second<>>;

using l1 = list<si::meter<>, si::second<>>;
using l2 = list<si::ampere<1, 99>, si::meter<>, si::kilogram<>>;

using s = merge_sum_sorted<l1, l2>;

// using e =
//     unit<double, scale<10>, si::meter<2, 1>, si::kilogram<>, si::second<>>;

// e emp{32};

int main() {
    kelvin a{1};
    degree d{8};
    // degree b{1};

    auto c = d / d;

    int i = c;

    std::cout << c.get() << std::endl;

    return 0;
}