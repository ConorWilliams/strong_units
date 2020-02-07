#include <iostream>

#include "unit2.hpp"

using kelvin = unit<double, scale<>, si::meter<>>;
using degree = unit<double, scale<2, 1, 273>, si::meter<>>;

using second = unit<double, scale<>, si::second<>>;
using minute = unit<double, scale<60>, si::second<>>;

int main() {
    kelvin a{0};
    degree b{1};

    a + b;

    // int i = b;

    std::cout << (a + b).get() << std::endl;

    return 0;
}