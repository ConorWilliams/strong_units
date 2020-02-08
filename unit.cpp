#include <iostream>

#include "unit2.hpp"

using xu::list;

using kelvin =
    unit<double, scale<>, si::meter<8>, si::kilogram<>, si::second<>>;
using degree =
    unit<double, scale<10, 1>, si::ampere<>, si::meter<>, si::second<>>;

using l1 = list<si::meter<>, si::second<>>;
using l2 = list<si::ampere<1, 99>, si::meter<>, si::kilogram<>>;

using s = xu::merge_sum_sorted_t<l1, l2>;

// using e =
//     unit<double, scale<10>, si::meter<2, 1>, si::kilogram<>, si::second<>>;

// e emp{32};

int main() {
    kelvin a{1};
    degree b{8};
    kelvin c{3};
    // degree b{1};

    // int i = a;

    c = a;

    auto d = a * a * c * a / a * a * c * b / b * a * a * a / b;

    // int i = d;

    std::cout << si::meter<>::symbol << std::endl;

    return 0;
}