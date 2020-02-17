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
template <std::intmax_t... Is> struct           substance : su::dimension<"mol", Is...> {};
template <std::intmax_t... Is> struct  luminous_intensity : su::dimension< "cd", Is...> {};

struct    scalar : su::symbol_unit<   scalar,      "scalar",        su::scale<>> {};

struct    metres : su::simple_unit<   metres, su::scale<2>,             length<>> {};
struct   seconds : su::simple_unit<  seconds, su::scale<>,               time<>> {};
struct kilograms : su::simple_unit<kilograms, su::scale<>,               mass<>> {};
struct   amperes : su::simple_unit<  amperes, su::scale<>,            current<>> {};
struct    kelvin : su::simple_unit<   kelvin, su::scale<>,         temprature<>> {};
struct     moles : su::simple_unit<    moles, su::scale<>,          substance<>> {};
struct  candelas : su::simple_unit< candelas, su::scale<>, luminous_intensity<>> {};


struct     hertz : su::symbol_unit<hertz, "Hz", su::scale<>, time<-1>> {};

struct kilometres : su::symbol_unit<kilometres, "km",  su::scale<1, 7, 3>,  length<>> {};

struct meters_per_second : su::simple_unit<meters_per_second, su::scale<>, length<>, time<-1>  > {};


}  // namespace si

#include <numeric>

using su::quantity;

struct a : su::symbol_unit<a, "a", su::scale<2, 1, 4>, si::length<>> {};
struct b : su::symbol_unit<b, "b", su::scale<1, 1, 9>, si::length<>> {};

struct   feet : su::symbol_unit<  feet,   "ft", su::scale<3048, 10000>, si::length<>> {};
struct inches : su::scaled_unit<inches, "inch",       su::scale<1, 12>,         feet> {};

//struct tw :  su::scaled_unit<tw, "w12", su::scale<12>,   si::metres> {};
struct tw : su::symbol_unit<tw, "a", su::scale<1>, si::length<>> {};
struct ti : su::symbol_unit<ti, "b", su::scale<31415926, 10000000, 0>, si::length<>> {};

// clang-format on

inline constexpr quantity<si::candelas, double> bear_oclock{12.};

// quantity operators

#include <iomanip>
#include <vector>

#include "src/math.hpp"

int main() {
    quantity<ti> a{1.};
    quantity<tw> b{1.};

    // std::cout << (quantity<tw>{1.} + quantity<ti>{1.}) << std::endl;

    std::cout << a - b << std::endl;
    std::cout << b + a << std::endl;
    // //
    // std::cout << quantity<si::metres>{quantity<tw>{1.} + quantity<ti>{1.}};

    // using help = su::common_help<si::metres, double, tw, double>;

    // std::cout << help::num << '/' << help::den << '^' << help::exp <<
    // std::endl;

    // int j = quantity<inches>::unit::scale_factor{};
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
