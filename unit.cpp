#include <iostream>

#include "unit2.hpp"

using kelvin = unit<double, scale<>, si::meter<>>;
using degree = unit<double, scale<2, 1, 273>, si::meter<>>;

using second = unit<double, scale<>, si::second<>>;
using minute = unit<double, scale<1, 1, 1, 1>, si::second<1>>;

using l = list<si::meter<>, si::second<>, si::meter<>, si::second<4>>;

using t = find<0, si::second<>,
               list<si::meter<>, si::second<>, si::meter<>, si::second<4>>>;

int main() {
    kelvin a{0};
    degree b{1};

    // a + b;

    int i = head<0, l>{};

    std::cout << t::value << std::endl;

    return 0;
}