// #pragma once

// #include <algorithm>
// #include <cstdint>
// #include <numeric>
// #include <ratio>
// #include <type_traits>

// #include "meta.hpp"

// class str_const {
//    private:
//     const char* const p_;
//     const std::size_t sz_;

//    public:
//     template <std::size_t N>
//     constexpr str_const(const char (&a)[N]) : p_(a), sz_(N - 1) {}

//     constexpr char operator[](std::size_t n) {
//         return n < sz_ ? p_[n] : throw std::out_of_range("");
//     }

//     constexpr std::size_t size() { return sz_; }
// };

// constexpr int str_compare(str_const a, str_const b) {
//     if (a.size() < b.size()) {
//         return -1;
//     } else if (b.size() > a.size()) {
//         return 1;
//     } else {
//         for (std::size_t i = 0; i < a.size(); ++i) {
//             if (a[i] < b[i]) {
//                 return -1;
//             } else if (a[i] > b[i]) {
//                 return 1;
//             }
//         }

//         return 0;
//     }
// }

// constexpr std::intmax_t sign(std::intmax_t x) { return x < 0 ? -1 : 1; }
// constexpr std::intmax_t absol(std::intmax_t x) { return sign(x) * x; }

// template <typename T>
// concept Dimension = requires(T x) {
//     x.num;
//     x.den;
//     x.symbol;
// };

// template <auto x>
// concept Dimension_v = Dimension<decltype(x)>;

// template <typename T>
// inline constexpr bool is_one_v = std::ratio_equal_v<std::ratio<1, 1>, T>;

// template <typename T>
// inline constexpr bool is_zero_v = std::ratio_equal_v<std::ratio<0, 1>, T>;

// ///////////////////////////// SCALE //////////////////////////////////

// template <std::intmax_t... Is>
// struct scale;

// template <>
// struct scale<> {
//     using scal = std::ratio<1, 1>;
//     using ofst = std::ratio<0, 0>;
// };

// template <std::intmax_t I>
// struct scale<I> {
//     using scal = std::ratio<I, 1>;
//     using ofst = std::ratio<0, 0>;
// };

// template <std::intmax_t I, std::intmax_t J>
// struct scale<I, J> {
//     using scal = std::ratio<I, J>;
//     using ofst = std::ratio<0, 0>;
// };

// template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
// struct scale<I, J, K> {
//     using scal = std::ratio<I, J>;
//     using ofst = std::ratio<K, 1>;
// };

// template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
// struct scale<I, J, K, L> {
//     using scal = std::ratio<I, J>;
//     using ofst = std::ratio<K, L>;
// };

// template <typename>
// struct is_scale : std::false_type {};

// template <std::intmax_t... Is>
// struct is_scale<scale<Is...>> : std::true_type {};

// template <typename T>
// concept Scale = is_scale<T>::value;

// //////////////////////////////   UNIT   ////////////////////////////////////

// template <Dimension First, Dimension Second, Dimension... Tail>
// constexpr bool ordered_impl(First first, Second second, Tail... tail) {
//     if constexpr (str_compare(first.symbol, second.symbol) < 0) {
//         if constexpr (sizeof...(Tail) == 0) {
//             return true;
//         } else {
//             return ordered(second, tail...);
//         }
//     } else {
//         return false;
//     }
// }

// // Verify that all Dims are ordered according to their symbol e.g
// // s0 < s1 && s1 < s2 && ... && s(n-1) < s(n)
// template <Dimension... Dims>
// constexpr bool ordered(Dims... dims) {
//     if constexpr (sizeof...(Dims) < 2) {
//         return true;
//     } else
//         return ordered_impl(dims...);
// }

// template <Dimension... Dims>
// constexpr bool non_zero(Dims... dims) {
//     return (... && (dims.num != 0)) && (... && (dims.den != 0));
// }

// template <typename T>
// concept Arithmetic = std::is_arithmetic_v<T>;

// template <typename T>
// concept Unit_t = requires {
//     typename T::dimension;
// };

// template <Arithmetic T, Scale S, Dimension_v... Dims>
//     requires ordered(Dims...) && non_zero(Dims...) struct Unit {
//     T value;

//     // TODO: https://en.cppreference.com/w/cpp/language/operator_arithmetic

//     template <Arithmetic oT, Scale oS>
//     inline constexpr auto operator+(Unit<oT, oS, Dims...> other) {
//         return Unit<decltype(value + other.value), scale<>, Dims...>{
//             value + other.value};
//     }
// };

// /////////// unit / dimension examples ////////////////

// struct meter {
//     static constexpr str_const symbol = "m";

//     const std::intmax_t num;
//     const std::intmax_t den;

//     constexpr meter(std::intmax_t Num, std::intmax_t Denom = 1)
//         : num{sign(Num) * sign(Denom) * absol(Num) / std::gcd(Num, Denom)},
//           den{absol(Denom) / std::gcd(Num, Denom)} {}
// };

// struct second {
//     static constexpr str_const symbol = "s";

//     const std::intmax_t num;
//     const std::intmax_t den;

//     constexpr second(std::intmax_t Num, std::intmax_t Denom = 1)
//         : num{sign(Num) * sign(Denom) * absol(Num) / std::gcd(Num, Denom)},
//           den{absol(Denom) / std::gcd(Num, Denom)} {}
// };
