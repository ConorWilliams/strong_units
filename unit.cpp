#include <iostream>

#include "src/quantity.hpp"
#include "src/si.hpp"

// TODO : check all pass by value no const
// TODO : static constexpr in functions

#include <numeric>

using su::quantity;
using namespace si;

int main() {
    quantity<second> s{1.};
    quantity<minute> m{1.};
    quantity<hour> h{1.};
    quantity<day> d{1.};
    quantity<year> y{1.};

    quantity<ohm, int> oh{3};

    // int i = decltype(d)::unit::scale_factor{};
    // int j = decltype(h)::unit::scale_factor{};

    auto [i, j, k] = su::gcd_frac(216, 25, 4, 18, 5, 3);

    std::cout << i << ' ' << j << ' ' << k << std::endl;

    std::cout << (s / oh) << std::endl;

    // std::cout << std::setprecision(30) << a * b << std::endl;
    // std::cout << a + a << std::endl;

    // using k = su::dimension_pack_simplify_t<si::length<1>, si::length<3>>;

    // int i = k{};
    // //
    // std::cout << quantity<si::metres>{quantity<tw>{1.} + quantity<ti>{1.}};

    // using help = su::common_help<si::metres, double, tw, double>;

    // std::cout << help::num << '/' << help::den << '^' << help::exp <<
    // std::endl;

    // int j = help::irreg_quantity{};
    // int i = decltype(b)::unit::scale_factor{};

    // int num1 = 5;
    // int den1 = 1;
    // int Exp1 = -1;

    // int num2 = 1;
    // int den2 = 1;
    // int Exp2 = 0;

    // std::intmax_t gcd_num = std::gcd(num1, num2);
    // std::intmax_t gcd_den = std::lcm(den1, den2);

    // constexpr auto t = su::gcdpow(8, 80808, 909);

    // std::cout << "mod time " << su::gcdpow(3, 4, 756) << std::endl;

    // constexpr auto k = su::gcd_frac(25, 909, -2, 1, 6, 0);

    // std::cout << gcd_num << '/' << gcd_den << "x10^" << std::min(Exp1, Exp2)
    //           << std::endl;

    return 0;
}
