#include <iostream>

#include "src/quantity.hpp"

// namespace su::nope {

// #pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wnon-template-friend"

// template <typename BaseType>
// struct downcast_base {
//     using base_type = BaseType;

//     friend auto downcast_guide(downcast_base);
// };

// #pragma GCC diagnostic pop

// template <Scale S, typename... Ts>
// struct UNIT : downcast_base<UNIT<S, Ts...>> {};

// template <typename T>
// concept Downcastable = requires {
//     typename T::base_type;
// }
// &&std::is_base_of_v<downcast_base<typename T::base_type>, T>;

// template <typename Target, Downcastable T>
// struct downcast_helper : T {
//     friend auto downcast_guide(typename downcast_helper::downcast_base) {
//         return Target();
//     }
// };

// template <typename Child, typename... Es>
// struct derived_dimension : downcast_helper<Child, UNIT<Es...>> {};

// template <typename T>
// concept has_downcast = requires {
//     downcast_guide(std::declval<downcast_base<T>>());
// };

// template <typename T>
// constexpr auto downcast_target_impl() {
//     if constexpr (has_downcast<T>) {
//         return decltype(downcast_guide(std::declval<downcast_base<T>>()))();
//     } else {
//         return T{};
//     }
// }

// template <Downcastable T>
// using downcast_target = decltype(downcast_target_impl<T>());

// ////////////////////////////////////////////////////////

// template <template <typename> typename CRTP, fs::fixed_string Sym, Arithmetic
// T,
//           Scale S, Dimension... Dims>

// struct named_unit : unit_make<T, S, Dims...> {
//     static constexpr auto self = named_unit<CRTP, Sym, T, S, Dims...>{};

//     using unit_make<T, S, Dims...>::unit;
//     using unit_make<T, S, Dims...>::operator=;

//     [[nodiscard]] inline static constexpr std::string_view symbol() noexcept
//     {
//         return m_symbol.view();
//     }

//     static constexpr fs::fixed_string m_symbol = Sym;
// };

// struct timer : derived_dimension<timer, scale<>, si::time<>> {};

// struct velocity : derived_dimension<velocity, scale<>, length<>> {};

// // struct minute: named_unit<minute, "min", su::scale<60>, si::second<>>;

// // quant<minute, double> elapsed_time;

// // quant<hour> final = elapsed_time

// template <typename T>
// struct conor : named_unit<conor, "conors", double, scale<>, length<>> {
//     using named_unit<conor, "conors", double, scale<>, length<>>::named_unit;
//     using named_unit<conor, "conors", double, scale<>, length<>>::operator=;
// };

// }  // namespace su::nope

namespace si {

// TODO : check all pass by value no const
// TODO : static constexpr in functions

// clang-format off
template <std::intmax_t... Is> struct              length : su::DimensionBase<  "m", Is...> {};
template <std::intmax_t... Is> struct                time : su::DimensionBase<  "s", Is...> {};
template <std::intmax_t... Is> struct                mass : su::DimensionBase< "kg", Is...> {};
template <std::intmax_t... Is> struct             current : su::DimensionBase<  "A", Is...> {};
template <std::intmax_t... Is> struct          temprature : su::DimensionBase<  "K", Is...> {};
template <std::intmax_t... Is> struct              amount : su::DimensionBase<"mol", Is...> {};
template <std::intmax_t... Is> struct  luminous_intensity : su::DimensionBase< "cd", Is...> {};

struct    scalar : su::named_unit<   scalar,      "scalar",        su::scale<>> {};

struct    metres : su::unit<   metres, su::scale<>,             length<>> {};
struct   seconds : su::unit<  seconds, su::scale<>,               time<>> {};
struct kilograms : su::unit<kilograms, su::scale<>,               mass<>> {};
struct   amperes : su::unit<  amperes, su::scale<>,            current<>> {};
struct    kelvin : su::unit<   kelvin, su::scale<>,         temprature<>> {};
struct     moles : su::unit<    moles, su::scale<>,             amount<>> {};
struct  candelas : su::unit< candelas, su::scale<>, luminous_intensity<>> {};


struct     hertz : su::named_unit<hertz, "Hz", su::scale<>, time<-1>> {};

struct kilometres : su::named_unit<kilometres, "km",  su::scale<1,1,3>,  length<>> {};

struct meters_per_second : su::unit<meters_per_second, su::scale<>, length<>, time<-1>  > {};

// clang-format on

}  // namespace si

#include <numeric>

using su::quantity;

struct a : su::named_unit<a, "a", su::scale<30, 7, -1>, si::length<>> {};
struct b : su::named_unit<b, "b", su::scale<5, 2>, si::length<>> {};

inline constexpr quantity<si::seconds, double> bear_oclock{12.};

// quantity operators

int main() {
    quantity<a, double> m{1.};

    quantity<b, double> km{1.};

    quantity<si::metres> sum = m + km;

    // int i = km + m;

    std::cout << m + km << ' ' << (m + km).base_symbol() << std::endl;
    std::cout << km << ' ' << (km).symbol() << std::endl;
    std::cout << sum << ' ' << (sum).symbol() << std::endl;

    //
    int num1 = 51;
    int den1 = 10;
    int Exp1 = 1;

    int num2 = 5;
    int den2 = 1;
    int Exp2 = 0;

    std::intmax_t gcd_num = std::gcd(num1, num2);
    std::intmax_t gcd_den = std::gcd(den1, den2);

    std::cout << gcd_num << '/' << (den1 / gcd_den) * den2 << '^'
              << std::min(Exp1, Exp2) << std::endl;

    return 0;
}
