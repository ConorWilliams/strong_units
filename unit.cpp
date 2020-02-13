#include <iostream>

#include "unit.hpp"

using kelvin = unit::unit_make<double, unit::scale<0, 2>>;

using gigaconors = unit::unit_scaled<si::prefix::nano, kelvin>;

using nanoconors = unit::unit_make<double, si::prefix::nano, si::meter_d<2>,
                                   si::second_d<>, si::kilogram_d<>>;

using gram = unit::unit_scaled<unit::scale_make<-3>, si::kilogram<int>>;

// using scalar = unit::unit<int, unit::scale<100>>;

using angstrom = unit::unit_make<double, unit::scale<-10>>;

using unit::list;

using l1 = unit::list<si::meter_d<>>;
using l2 = unit::list<si::ampere_d<>, si::second_d<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

template <typename T>
using scalar = unit::unit_make<T, unit::scale<-9>>;

using unit::Unit;

// int i = kelvin{};

int main() {
    kelvin a{10.};

    std::cout << scalar<int>{56} << std::endl;

    a = (a + a);

    std::cout << si::meter << std::endl;

    nanoconors con{1.};

    nanoconors b{1.};

    // double hj = scalar<double>{3.};

    // std::cout << a.get() << ' ' << con.get() << std::endl;

    Unit e = a * a / con;

    return 0;
}