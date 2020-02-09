#include <iostream>

#include "unit.hpp"

using conors = unit::make<double, unit::scale<>, si::meter<1>>;

using gigaconors = unit::make_scaled<unit::scale<1000>, conors>;

int main() {
    // k = unit::raw_convert<kelvin>(c);

    int b, k, c;

    conors a{10.};

    auto d = c / b * b / c;

    unit::make<double, unit::scale<1>, si::meter<2>> e =
        a * a * a / gigaconors{1.};

    std::cout << e << std::endl;

    return 0;
}