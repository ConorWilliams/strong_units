#include <iostream>

#include "unit.hpp"

using kelvin = unit::make<double, unit::scale<>, si::mole<>, si::kilogram<>,
                          si::meter<>, si::second<>, si::mole<>, si::meter<>>;

kelvin a{};

using gigaconors = unit::make_scaled<unit::scale<1000>, kelvin>;

using scalar = unit::unit<int, unit::scale<>, si::meter<0>>;

using unit::list;

using l1 = unit::list<si::meter<>>;
using l2 = unit::list<si::second<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

// int i = m{};

int main() {
    int b, k, c;

    kelvin a{10.};

    auto d = c / b * b / c;

    auto e = a * a * a / gigaconors{1.};

    std::cout << e << std::endl;

    return 0;
}