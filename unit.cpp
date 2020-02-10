#include <iostream>

#include "unit.hpp"

using kelvin = unit::make<
    double, unit::scale<>, si::_mole<>, si::_kilogram<>, si::_meter<>,
    si::_second<>, si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>,
    si::_meter<2>, si::_second<>, si::_kilogram<>, si::_candela<99>,
    si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>,
    si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>,
    si::_kilogram<>, si::_candela<99>, si::_mole<>, si::_kilogram<>,
    si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>, si::_mole<-1>,
    si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_ampere<9>, si::_meter<>, si::_second<>, si::_mole<-1>,
    si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>,
    si::_kilogram<>, si::_candela<99>, si::_mole<>, si::_kilogram<>,
    si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>, si::_mole<-1>,
    si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_kelvin<-9>, si::_candela<99>, si::_mole<>, si::_kilogram<>,
    si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>, si::_mole<-1>,
    si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>, si::_mole<>, si::_kilogram<>, si::_meter<>, si::_second<>,
    si::_mole<-1>, si::_meter<>, si::_mole<-1>, si::_meter<>, si::_meter<2>,
    si::_second<>, si::_kilogram<>, si::_candela<99>, si::_mole<>,
    si::_kilogram<>, si::_meter<>, si::_second<>, si::_mole<-1>, si::_meter<>,
    si::_mole<-1>, si::_meter<>, si::_meter<2>, si::_second<>, si::_kilogram<>,
    si::_candela<99>>;

using gigaconors = unit::make_scaled<si::prefix::nano, kelvin>;

using nanoconors = unit::unit<double, si::prefix::nano, si::_meter<2>,
                              si::_second<>, si::_kilogram<>>;

using gram = unit::make_scaled<unit::scale_make<-3>, si::kilogram<int>>;

// using scalar = unit::unit<int, unit::scale<100>>;

using angstrom = unit::make<double, unit::scale<-10>>;

using unit::list;

using l1 = unit::list<si::_meter<>>;
using l2 = unit::list<si::_ampere<>, si::_second<>>;

using m = unit::merge_sum_sorted_t<l1, l2>;

template <typename T>
using scalar = unit::make<T, unit::scale<>>;

using unit::Unit;

int i = kelvin{};

int main() {
    kelvin a{10.};

    constexpr scalar<int> s{3};

    // std::cout << a.get() << std::endl;

    a = (a + a);

    // std::cout << a.get() << std::endl;

    nanoconors con{1.};

    nanoconors b{1.};

    // double hj = scalar<double>{3.};

    // std::cout << a.get() << ' ' << con.get() << std::endl;

    Unit e = a * a / con;

    Unit h = (s + scalar<double>{4.}) * e;

    // int i = e;

    std::cout << gram{3} + si::kilogram<int>{1} << ' ' << h << std::endl;

    return 0;
}