#include <iostream>

#include "unit.hpp"

using kelvin =
    unit::make<double, unit::scale<>, si::_mole<>, si::_kilogram<>,
               si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>>;

using gigaconors = unit::make_scaled<si::prefix::nano, kelvin>;

using nanoconors = unit::unit<double, si::prefix::nano, si::_meter<2>,
                              si::_second<>, si::_kilogram<>>;

using scalar = unit::unit<int, unit::scale<>, si::_meter<0>>;

using unit::list;

using l1 = unit::list<si::_meter<>>;
using l2 = unit::list<si::_ampere<>, si::_second<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

// int i = m{};

int main() {
    int b, k, c;

    kelvin a{10.};

    a = (a + a);

    auto d = c / b * b / c;

    nanoconors con{1.};

    // con + d;

    // auto e = a * a * b / con;

    std::cout << a << std::endl;

    return 0;
}