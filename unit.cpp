#include <iostream>

#include "unit2.hpp"

using kelvin = unit<double, scale<>, si::meter<>, si::second<>>;
using degree = unit<double, scale<>, si::ampere<>, si::kilogram<>>;

using l1 = list<si::meter<>, si::second<>>;
using l2 = list<si::ampere<>, si::kilogram<>>;

using s = merge_sum_sorted<l1, l2>;

int main() {
    kelvin a{0};
    degree b{1};

    int i = s{};

    std::cout << 1 << std::endl;

    return 0;
}