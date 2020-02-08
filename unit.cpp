#include <iostream>

#include "unit2.hpp"

using xu::list;

using kelvin =
    unit<double, scale<>, si::meter<8, 9>, si::kilogram<>, si::second<>>;
using degree =
    unit<double, scale<10, 1>, si::meter<8, 9>, si::kilogram<>, si::second<>>;

using conors = unit<double, scale<>, si::meter<>>;

using l1 = list<si::meter<>, si::second<>>;
using l2 = list<si::ampere<1, 99>, si::meter<>, si::kilogram<>>;

using s = xu::merge_sum_sorted_t<l1, l2>;

// using e =
//     unit<double, scale<10>, si::meter<2, 1>, si::kilogram<>, si::second<>>;

// e emp{32};

int main() {
    kelvin a{1.};
    degree b{1.};
    conors c{3.};

    kelvin k = b;

    // k = xu::raw_convert<kelvin>(c);

    auto d = c / b * b / c;

    auto e = a * a * a;

    std::cout << k << std::endl;

    return 0;
}