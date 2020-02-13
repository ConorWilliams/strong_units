#include <iostream>

#include "si.hpp"

using gigaconors = unit::unit_scaled<prefix::nano, kelvin_t<int>>;

using nanoconors =
    unit::unit_make<double, prefix::nano, meter_d<2>, second_d<>, kilogram_d<>>;

using gram = unit::unit_scaled<unit::scale_make<-3>, kilogram_t<int>>;

// using scalar = unit::unit<int, unit::scale<100>>;

using angstrom = unit::unit_make<double, unit::scale<-10>>;

using unit::list;

using l1 = unit::list<meter_d<>>;
using l2 = unit::list<ampere_d<>, second_d<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

template <typename T>
using scalar = unit::unit_make<T, unit::scale<-9>>;

using unit::Unit;

// int i = kelvin{};

int main() {
    kelvin_t<double> a{10.};

    std::cout << scalar<int>{56} << std::endl;

    a = (a + a);

    std::cout << (meter) << std::endl;

    nanoconors con{1.};

    nanoconors b{1.};

    // double hj = scalar<double>{3.};

    // std::cout << a.get() << ' ' << con.get() << std::endl;

    Unit e = a * a / con;

    return 0;
}