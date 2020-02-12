#include <iostream>

#include "unit.hpp"

using kelvin = unit::make<double, unit::scale<0, 2>>;

using gigaconors = unit::make_scaled<si::prefix::nano, kelvin>;

using nanoconors = unit::unit<double, si::prefix::nano, si::meter_d<2>,
                              si::second_d<>, si::kilogram_d<>>;

using gram = unit::make_scaled<unit::scale_make<-3>, si::kilogram<int>>;

// using scalar = unit::unit<int, unit::scale<100>>;

using angstrom = unit::make<double, unit::scale<-10>>;

using unit::list;

using l1 = unit::list<si::meter_d<>>;
using l2 = unit::list<si::ampere_d<>, si::second_d<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

template <typename T>
using scalar = unit::make<T, unit::scale<>>;

using unit::Unit;

// int i = kelvin{};

int main() {
    kelvin a{10.};

    std::cout << fs::ito_fs<3455> << std::endl;

    constexpr scalar<int> s{3};

    si::kilo<scalar<int>> km{};

    using kilometer = si::kilo<scalar<int>>;

    std::cout << scalar<int>{56} << "K" << std::endl;

    a = (a + a);

    std::cout << si::meter << std::endl;

    nanoconors con{1.};

    nanoconors b{1.};

    // double hj = scalar<double>{3.};

    // std::cout << a.get() << ' ' << con.get() << std::endl;

    Unit e = a * a / con;

    // int i = e;

    Unit h = (s + scalar<double>{4.}) * e;

    // int i = e;

    std::cout << gram{3} + si::kilogram<int>{1} << ' ' << h << std::endl;

    return 0;
}