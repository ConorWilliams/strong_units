#include <iostream>

#include "src/quantity.hpp"

namespace si {

// TODO : check all pass by value no const
// TODO : static constexpr in functions

// clang-format off
template <std::intmax_t... Is> struct              length : su::dimension<  "m", Is...> {};
template <std::intmax_t... Is> struct                time : su::dimension<  "s", Is...> {};
template <std::intmax_t... Is> struct                mass : su::dimension< "kg", Is...> {};
template <std::intmax_t... Is> struct             current : su::dimension<  "A", Is...> {};
template <std::intmax_t... Is> struct          temprature : su::dimension<  "K", Is...> {};
template <std::intmax_t... Is> struct              amount : su::dimension<"mol", Is...> {};
template <std::intmax_t... Is> struct  luminous_intensity : su::dimension< "cd", Is...> {};

struct    scalar : su::named_unit<   scalar,      "scalar",        su::scale<>> {};

struct    metres : su::unit<   metres, su::scale<>,             length<>> {};
struct   seconds : su::unit<  seconds, su::scale<>,               time<>> {};
struct kilograms : su::unit<kilograms, su::scale<>,               mass<>> {};
struct   amperes : su::unit<  amperes, su::scale<>,            current<>> {};
struct    kelvin : su::unit<   kelvin, su::scale<>,         temprature<>> {};
struct     moles : su::unit<    moles, su::scale<>,             amount<>> {};
struct  candelas : su::unit< candelas, su::scale<>, luminous_intensity<>> {};


struct     hertz : su::named_unit<hertz, "Hz", su::scale<>, time<-1>> {};

struct kilometres : su::named_unit<kilometres, "km",  su::scale<1, 7, 3>,  length<>> {};

struct meters_per_second : su::unit<meters_per_second, su::scale<>, length<>, time<-1>  > {};

// clang-format on

}  // namespace si

#include <numeric>

using su::quantity;

struct a : su::named_unit<a, "a", su::scale<2, 1, 4>, si::length<>> {};
struct b : su::named_unit<b, "b", su::scale<1, 1, 2>, si::length<>> {};

inline constexpr quantity<si::candelas, double> bear_oclock{12.};

// quantity operators

#include <iomanip>
#include <vector>

int main() {
    quantity<si::metres, double> m{5.};

    constexpr quantity<si::kilometres, double> km{3.};

    quantity<si::kelvin> sum{3.};

    // auto t = km * sum;

    // bool res = m == km;
    auto t = km + m;
    // quantity<si::kilometres, double> c = m + km;

    std::cout << (t) << std::endl;
    // std::cout << t << std::endl;
    // std::cout << std::setprecision(6) << km + m << std::endl;

    //
    // int num1 = 3;
    // int den1 = 1;
    // int Exp1 = 1;

    // int num2 = 20;
    // int den2 = 7;
    // int Exp2 = 0;

    // std::intmax_t gcd_num = std::gcd(num1, num2);
    // std::intmax_t gcd_den = std::gcd(den1, den2);

    // std::cout << gcd_num << '/' << (den1 / gcd_den) * den2 << "x10^"
    //           << std::min(Exp1, Exp2) << std::endl;

    return 0;
}
