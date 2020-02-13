#pragma once

// Copyright (c) 2020 Conor Williams

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <cmath>    // pow
#include <cstddef>  // std::size_t
#include <cstdint>  // std::intmax_t
#include <ostream>
#include <ratio>
#include <stdexcept>
#include <type_traits>

#include "fixed_string.hpp"

namespace unit {

// Lightweight list type required to separate parameter packs
template <typename... Ts>
struct list {
    inline static constexpr std::size_t size() { return sizeof...(Ts); }
};

namespace detail {

template <typename>
struct is_list : std::false_type {};

template <typename... Ts>
struct is_list<list<Ts...>> : std::true_type {};

}  // namespace detail

template <typename T>
concept List = detail::is_list<T>::value;

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

// Convenience struct for inheriting: using type = ...
template <typename T = void>
struct Type {
    using type = T;
};

namespace detail {

struct scale_tag {};  // marks class as being of scale type

}  // namespace detail

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct ScaleBase : private detail::scale_tag {
    using ratio = std::ratio<J, K>;

    static constexpr std::intmax_t exp = I;
    static constexpr std::intmax_t num = ratio::num;
    static constexpr std::intmax_t den = ratio::den;

    static_assert(num > 0, "Cannot have zero or negative scaled dimension");
};

template <typename T>
concept Scale = std::is_base_of_v<detail::scale_tag, T>;

// Using a variadic template instead of defaulted results in shorter types

template <std::intmax_t... Is>
struct scale {
    static_assert(sizeof...(Is) < 4, "Scale has too many template arguments");
};

template <>
struct scale<> : ScaleBase<0, 1, 1> {};

template <std::intmax_t I>
struct scale<I> : ScaleBase<I, 1, 1> {};

template <std::intmax_t I, std::intmax_t J>
struct scale<I, J> : ScaleBase<I, J, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale<I, J, K> : ScaleBase<I, J, K> {};

namespace detail {

enum { multiply, done, divide };

template <typename Ratio>
constexpr int standard_direction() {
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-10>>) {
        return divide;
    }
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-1>>) {
        return done;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<0>>) {
        return multiply;
    }
    if constexpr (std::ratio_equal_v<Ratio, std::ratio<0>>) {
        return done;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<1>>) {
        return multiply;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<10>>) {
        return done;
    }
    if constexpr (std::ratio_greater_equal_v<Ratio, std::ratio<10>>) {
        return divide;
    }
}

// forward declaration for standard_form_impl
template <int C, std::intmax_t Exp, typename Ratio>
struct standard_match;

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl;

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl
    : standard_match<standard_direction<Ratio>(), Exp, Ratio> {};

// end condition
template <std::intmax_t Exp, typename Ratio>
struct standard_match<done, Exp, Ratio> : Type<Ratio> {
    static constexpr std::intmax_t exp = Exp;
};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<divide, Exp, Ratio>
    : standard_form_impl<Exp + 1, std::ratio_divide<Ratio, std::ratio<10>>> {};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<multiply, Exp, Ratio>
    : standard_form_impl<Exp - 1, std::ratio_multiply<Ratio, std::ratio<10>>> {
};

}  // namespace detail

// convert a std::ratio to standard form, returns a struct with ::type = the
// standard form ratio and ::exp the base 10 exponent.
template <typename Ratio>
using standard_form = detail::standard_form_impl<0, Ratio>;

namespace detail {

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_impl : Type<scale<I, J, K>> {};

template <std::intmax_t I, std::intmax_t J>
struct scale_make_impl<I, J, 1> : Type<scale<I, J>> {};

template <std::intmax_t I>
struct scale_make_impl<I, 1, 1> : Type<scale<I>> {};

template <>
struct scale_make_impl<0, 1, 1> : Type<scale<>> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_help {
    using standard = standard_form<std::ratio<J, K>>;
    using type = scale_make_impl<I + standard::exp, standard::type::num,
                                 standard::type::den>::type;
};

}  // namespace detail

// Returns the most minimal possible scale type in standard form
template <std::intmax_t I = 0, std::intmax_t J = 1, std::intmax_t K = 1>
using scale_make = detail::scale_make_help<I, J, K>::type;

namespace detail {

template <Scale A, Scale B>
struct scale_multiply {
    using product = std::ratio_multiply<typename A::ratio, typename B::ratio>;

    using type = scale_make<A::exp + B::exp, product::num, product::den>;
};

template <Scale A, Scale B>
struct scale_divide {
    using product = std::ratio_divide<typename A::ratio, typename B::ratio>;

    using type = scale_make<A::exp - B::exp, product::num, product::den>;
};

}  // namespace detail

template <Scale A, Scale B>
using scale_multiply_t = detail::scale_multiply<A, B>::type;

template <Scale A, Scale B>
using scale_divide_t = detail::scale_divide<A, B>::type;

namespace detail {

// Constexpr pow10 function
template <std::intmax_t exponent>
constexpr std::conditional_t<(exponent < 308), double, long double>
pow10_impl() {
    if constexpr (exponent == 0) {
        return 1.;
    } else if constexpr (exponent == 1) {
        return 10.;
    } else if constexpr (exponent % 2 == 0) {
        return pow10_impl<exponent / 2>() * pow10_impl<exponent / 2>();
    } else {
        return pow10_impl<exponent / 2>() * pow10_impl<exponent / 2>() * 10;
    }
}

template <std::intmax_t exponent>
constexpr auto pow10() {
    if constexpr (exponent > 0) {
        return pow10_impl<exponent>();
    } else {
        return 1. / pow10_impl<-exponent>();
    }
}

}  // namespace detail

// Could be generalised to arbitrary scale<...> !
template <Scale From, Scale To, typename T>
inline constexpr auto scale_convert(T const x) {
    using conversion = scale_divide_t<From, To>;

    constexpr std::intmax_t num = conversion::num;
    constexpr std::intmax_t den = conversion::den;
    constexpr std::intmax_t exp = conversion::exp;

    // Avoid floating point multiplication without -ffast-math
    if constexpr (exp == 0) {
        if constexpr (num == 1 && den == 1) {
            return x;
        } else if constexpr (num != 1 && den == 1) {
            return x * num;
        } else if constexpr (num == 1 && den != 1) {
            return x / den;
        } else {
            return x * static_cast<double>(num) / den;
        }
    } else {
        // double or long double
        constexpr auto pow10 = detail::pow10<exp>();

        if constexpr (num == 1 && den == 1) {
            return x * pow10;
        } else if constexpr (num != 1 && den == 1) {
            return (x * num) * pow10;
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<double>(den) * pow10;
        } else {
            return x * static_cast<double>(num) / den * pow10;
        }
    }
}

//////////////////////////////   Dimension ////////////////////////////////////

namespace detail {

struct dimension_tag {};  // Marks class as being a dimension type.

}  // namespace detail

// Defaults required for variadic instantiation
template <fs::fixed_string Str, std::intmax_t I = 1, std::intmax_t J = 1>
struct DimensionBase : private detail::dimension_tag {
    using exp = std::ratio<I, J>;

    static constexpr std::intmax_t num = exp::num;
    static constexpr std::intmax_t den = exp::den;

    static constexpr fs::fixed_string symbol = Str;
};

template <typename T>
concept Dimension = std::is_base_of_v<detail::dimension_tag, T>;

namespace detail {

template <typename, typename>
struct dimension_same : std::false_type {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          std::intmax_t... Ir>
struct dimension_same<Dim<Il...>, Dim<Ir...>> : std::true_type {};

}  // namespace detail

// Test if two types are specialisations of the same dimension type.
template <Dimension A, Dimension B>
inline constexpr bool dimension_same_v = detail::dimension_same<A, B>::value;

namespace detail {

template <typename, typename>
struct dimension_equal : std::false_type {};

template <Dimension A, Dimension B>
struct dimension_equal<A, B> {
    static constexpr bool value =
        dimension_same_v<A, B> &&
        std::ratio_equal_v<typename A::exp, typename B::exp>;
};

template <Dimension... Dl, Dimension... Dr>
    requires sizeof...(Dl) ==
    sizeof...(Dr) struct dimension_equal<list<Dl...>, list<Dr...>> {
    static constexpr bool value =
        std::conjunction_v<dimension_equal<Dl, Dr>...>;
};

}  // namespace detail

// Test if two types are specialisations of the same dimension type and have
// equal exponents.
template <typename A, typename B>
inline constexpr bool dimension_equal_v = detail::dimension_equal<A, B>::value;

namespace detail {

template <typename, std::intmax_t, std::intmax_t>
struct dimension_simplify;

template <template <std::intmax_t...> typename Dim, std::intmax_t I,
          std::intmax_t J, std::intmax_t... Is>
struct dimension_simplify<Dim<Is...>, I, J> : Type<Dim<I, J>> {};

template <template <std::intmax_t...> typename Dim, std::intmax_t I,
          std::intmax_t... Is>
struct dimension_simplify<Dim<Is...>, I, 1> : Type<Dim<I>> {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is>
struct dimension_simplify<Dim<Is...>, 1, 1> : Type<Dim<>> {};

}  // namespace detail

// Converts a dimensions into its shortest possible version representation
template <Dimension D>
using dimension_simplify_t =
    detail::dimension_simplify<D, D::num, D::den>::type;

namespace detail {
template <typename, typename>
struct dimension_add;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is,
          typename Ratio>
struct dimension_add<Dim<Is...>, Ratio> {
    using sum = std::ratio_add<typename Dim<Is...>::exp, Ratio>;
    using type = dimension_simplify_t<Dim<sum::num, sum::den>>;
};

}  // namespace detail

// Returns new dimension with same type and new exponent equal to the sum of
// std::ratio O and argument dimensions' exponent.
template <Dimension D, typename Ratio>
using dimension_add_t = detail::dimension_add<D, Ratio>::type;

namespace detail {

template <typename, typename>
struct dimension_multiply;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is,
          typename Ratio>
struct dimension_multiply<Dim<Is...>, Ratio> {
    using product = std::ratio_multiply<typename Dim<Is...>::exp, Ratio>;
    using type = dimension_simplify_t<Dim<product::num, product::den>>;
};

}  // namespace detail

// Returns new dimension with same type but and new exponent equal to the
// product of std::ratio O and the argument dimensions' exponent.
template <Dimension D, typename Ratio>
using dimension_multiply_t = detail::dimension_multiply<D, Ratio>::type;

namespace detail {

template <typename>
struct ordered_impl;

template <>
struct ordered_impl<list<>> : std::true_type {};

template <Dimension D>
struct ordered_impl<list<D>> : std::true_type {};

template <Dimension First, Dimension Second, Dimension... Tail>
struct ordered_impl<list<First, Second, Tail...>> {
    static constexpr bool value =
        fs::compare(First::symbol, Second::symbol) < 0 &&
        ordered_impl<list<Second, Tail...>>::value;
};

}  // namespace detail

// Checks if a list<...> of dimensions satisfies strict ordering,
// e.g. d_n < d_n+1 == true for all n < N.
template <List L>
inline constexpr bool ordered_v = detail::ordered_impl<L>::value;

/////////////////////////// list<> meta  ////////////////////////

namespace detail {

template <typename, typename>
struct concat;

template <typename Head, typename... Tail>
struct concat<Head, list<Tail...>> : Type<list<Head, Tail...>> {};

template <typename... Head, typename Tail>
struct concat<list<Head...>, Tail> : Type<list<Head..., Tail>> {};

template <typename... Head, typename... Tail>
struct concat<list<Head...>, list<Tail...>> : Type<list<Head..., Tail...>> {};

}  // namespace detail

// Concatenates two lists or list and non list;
template <typename A, typename B>
    requires List<A> || List<B> using concat_t = detail::concat<A, B>::type;

namespace detail {

// Recursively concatenates A/B into L choosing 'smallest' each time to maintain
// order, dimensions comparing equal are summed
template <int, List L, List A, List B>
struct match;

template <List, List, List>
struct merge;

// Nothing left to merge end case
template <List L>
struct merge<L, list<>, list<>> : Type<L> {};

// End cases for different length lists
template <List L, Dimension... Dims>
struct merge<L, list<Dims...>, list<>> : concat<L, list<Dims...>> {};

// End cases for different length lists
template <List L, Dimension... Dims>
struct merge<L, list<>, list<Dims...>> : concat<L, list<Dims...>> {};

// General case performs string comparison and despatches to match<...>
template <List L, Dimension A, Dimension... As, Dimension B, Dimension... Bs>
struct merge<L, list<A, As...>, list<B, Bs...>>
    : match<compare(A::symbol, B::symbol), L, list<A, As...>, list<B, Bs...>> {
};

// Concatenate head of A into L
template <List L, Dimension A, Dimension... As, List B>
struct match<-1, L, list<A, As...>, B> : merge<concat_t<L, A>, list<As...>, B> {
};

// Concatenate head of B into L
template <List L, List A, Dimension B, Dimension... Bs>
struct match<1, L, A, list<B, Bs...>> : merge<concat_t<L, B>, A, list<Bs...>> {
};

// Equal comparison case means we sum the dimension exponents
template <List L, Dimension A, Dimension... As, Dimension B, Dimension... Bs>
struct match<0, L, list<A, As...>, list<B, Bs...>> {
    static_assert(dimension_same_v<A, B>, "Dimensions cannot have == symbols.");

    using dim_sum = dimension_add_t<A, typename B::exp>;

    using type = std::conditional_t<
        std::ratio_equal_v<typename dim_sum::exp, std::ratio<0>>,
        typename merge<L, list<As...>, list<Bs...>>::type,
        typename merge<concat_t<L, dim_sum>, list<As...>, list<Bs...>>::type>;
};

}  // namespace detail

// Merge two sorted lists of dimensions into a new sorted list summing any
// dimensions of the same type.
template <List A, List B>
requires ordered_v<A> &&ordered_v<B> using merge_sum_sorted_t =
    detail::merge<list<>, A, B>::type;

namespace detail {

// Bottom-up, compile time merge sorting!
template <List...>
struct sort_impl;

// Empty list (sorting nothing) end case
template <>
struct sort_impl<list<>> : Type<list<>> {};

// End condition = working list contains single (sorted) list
template <List Single>
struct sort_impl<list<Single>> : Type<Single> {};

// Re-curse condition = no more sub-list, expand working list and rerun
template <List... Ls>
struct sort_impl<list<Ls...>> : sort_impl<list<>, Ls...> {};

// Concatenate leftover (odd) sub-list into working list
template <List Working, List Odd>
struct sort_impl<Working, Odd> : sort_impl<concat_t<Working, list<Odd>>> {};

// General case - merge sub-lists and concatenate into working list
template <List Working, List First, List Second, List... Tail>
struct sort_impl<Working, First, Second, Tail...>
    : sort_impl<concat_t<Working, list<merge_sum_sorted_t<First, Second>>>,
                Tail...> {};

}  // namespace detail

// Compile time bottom-up merge sort a parameter pack of dimensions
template <Dimension... Dims>
using sort_t = detail::sort_impl<list<>, list<Dims>...>::type;

////////////////////////// String manipulations  //////////////////////////////

// Extracts symbol from dimension and returns symbol as a static string
// decorated with exponent in minimal "symbol^x/y" like form.
template <Dimension D>
inline constexpr auto anotate() {
    using namespace fs;  // for string literal operator

    constexpr std::intmax_t num = D::num;
    constexpr std::intmax_t den = D::den;

    if constexpr (num == 1 && den == 1) {
        return D::symbol;

    } else if constexpr (num != 1 && den == 1) {
        return D::symbol + "^"_fs + ito_fs<num>;

    } else if constexpr (den != 1) {
        return D::symbol + "^"_fs + ito_fs<num> + "/"_fs + ito_fs<den>;
    }
}

// Stringifys a scale<> into a minimal "(a/b x 10^c)" like form.
template <Scale S>
inline constexpr auto anotate() {
    using namespace fs;  // for string literal operator

    constexpr std::intmax_t num = S::num;
    constexpr std::intmax_t den = S::den;
    constexpr std::intmax_t exp = S::exp;

    if constexpr (exp == 0) {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return ""_fs;
            } else {
                return "("_fs + ito_fs<num> + ")"_fs;
            }
        } else {
            return "("_fs + ito_fs<num> + "/"_fs + ito_fs<den> + ")"_fs;
        }
    } else {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return "(10^"_fs + ito_fs<exp> + ")"_fs;
            } else {
                return "("_fs + ito_fs<num> + " x 10^"_fs + ito_fs<exp> +
                       ")"_fs;
            }
        } else {
            return "("_fs + ito_fs<num> + "/"_fs + ito_fs<den> + " x 10^"_fs +
                   ito_fs<exp> + ")"_fs;
        }
    }
}

// Join annotated scale and dimensions with correct spaces.
template <typename Head, typename... Tail>
inline constexpr auto join(Head head, Tail... tail) {
    using namespace fs;  // for string literal operator

    if constexpr (sizeof...(Tail) == 0) {
        if constexpr (Head::size() == 0) {
            return "dimensionless"_fs;
        } else {
            return head + " dimensionless"_fs;
        }
    } else {
        if constexpr (Head::size() == 0) {
            // Skip head, need ""_fs for case tail.size() = 1
            return join(tail..., ""_fs);
        } else {
            return fixed_string{head, (" "_fs + tail)...};
        }
    }
}

////////////////////////////////// UNIT  ////////////////////////////////////

// Forward declaration for concept
template <Arithmetic T, Scale S, Dimension... Dims>
class unit;

namespace detail {

template <typename>
struct is_unit : std::false_type {};

template <Arithmetic T, Scale S, Dimension... Dims>
struct is_unit<unit<T, S, Dims...>> : std::true_type {};

}  // namespace detail

template <typename T>
concept Unit = detail::is_unit<T>::value;

namespace detail {

template <Arithmetic, Scale, typename>
struct unit_make_from_sorted;

template <Arithmetic T, Scale S, Dimension... Dims>
struct unit_make_from_sorted<T, S, list<Dims...>> : Type<unit<T, S, Dims...>> {
};

}  // namespace detail

// Helper to make a unit from a dimension list<...>.
template <Arithmetic T, Scale S, List L>
using unit_make_from_sorted_t = detail::unit_make_from_sorted<T, S, L>::type;

// Main way for users to make units. Makes a unit type by simplifying and
// sorting the dimensions and simplifying the scale.
template <Arithmetic T, Scale S, Dimension... Dims>
using unit_make =
    unit_make_from_sorted_t<T, scale_make<S::exp, S::num, S::den>,
                            sort_t<dimension_simplify_t<Dims>...>>;

// Secondary way for users to make units. Makes a unit type by rescaling an
// existing unit type.
template <Scale S, Unit U>
using unit_scaled =
    unit_make_from_sorted_t<typename U::value_type,
                            scale_multiply_t<S, typename U::scale_factor>,
                            typename U::dimensions>;

// Safe unit conversion but returns raw float, double, etc
template <Unit To, Unit From>
[[nodiscard]] constexpr auto raw_convert(
    From const x) requires dimension_equal_v<To, From> {
    return scale_convert<typename From::scale_factor,
                         typename To::scale_factor>(x.get());
}

// Dimension safe unit conversion -- arithmetic type of unit may not equal
// To::value_type e.g. decltype(return) != To in all cases.
template <Unit To, Unit From>
[[nodiscard]] constexpr auto convert(
    From const x) requires dimension_equal_v<To, From> {
    return unit_make_from_sorted_t<decltype(raw_convert<To>(x)),
                                   typename To::scale_factor,
                                   typename To::dimensions>{raw_convert<To>(x)};
}

// Overloading to accept unit<> which holds ::dimension = list<Dims...>
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

// The libraries central type.
template <Arithmetic T, Scale S, Dimension... Dims>
class unit {
   public:
    using value_type = T;
    using scale_factor = S;
    using dimensions = list<Dims...>;

    static_assert((... && (Dims::num != 0)),
                  "Unit dimension exponents cannot be zero.");

    static_assert(ordered_v<dimensions>,
                  "Unit dimensions must satisfy strict ordering.");

    unit() = default;
    unit(unit const &) = default;
    unit(unit &&) = default;

    unit &operator=(unit const &) = default;
    unit &operator=(unit &&) = default;

    constexpr explicit unit(Arithmetic value) : m_value{value} {};

    template <Unit U>
    requires dimension_equal_v<unit, U> constexpr explicit unit(U const &other)
        : m_value{raw_convert<unit>(other)} {}

    template <Unit U>
    requires dimension_equal_v<unit, U> constexpr unit &operator=(
        U const &other) {
        m_value = raw_convert<unit>(other);
        return *this;
    }

#ifdef UNIT_SCALAR_IMPLICIT_CONVERSION
    // Implicit conversions are dangerous but useful for scalar -> sine(x) etc.
    inline constexpr operator value_type() const requires S::num == 1 &&
        S::den == 1 && S::exp == 0 && sizeof...(Dims) == 0 {
        return m_value;
    }
#endif

    [[nodiscard]] inline constexpr value_type get() const noexcept {
        return m_value;
    }

    [[nodiscard]] inline static constexpr std::string_view symbol() noexcept {
        return m_symbol.view();
    }

   private:
    value_type m_value;

    // Symbol contains scale info and dimension symbols / exponents.
    static constexpr fs::fixed_string m_symbol =
        join(anotate<S>(), anotate<Dims>()...);
};

// Deduction to default to scalar unit.
template <typename T>
unit(T)->unit<T, scale<>>;

// cout unit
std::ostream &operator<<(std::ostream &os, const Unit &obj) {
    return os << obj.get();
}

///////////////////////////  operators ////////////////////////////

template <Unit A, Unit B>
constexpr inline auto operator+(A const lhs,
                                B const rhs) requires dimension_equal_v<A, B> {
    using unit_t =
        unit_make_from_sorted_t<decltype(lhs.get() + raw_convert<A>(rhs)),
                                typename A::scale_factor,
                                typename A::dimensions>;

    return unit_t{lhs.get() + raw_convert<A>(rhs)};
}

template <Unit A, Unit B>
constexpr inline auto operator-(A lhs, B rhs) requires dimension_equal_v<A, B> {
    using unit_t =
        unit_make_from_sorted_t<decltype(lhs.get() - raw_convert<A>(rhs)),
                                typename A::scale_factor,
                                typename A::dimensions>;

    return unit_t{lhs.get() - raw_convert<A>(rhs)};
}

template <Unit A, Unit B>
constexpr inline auto operator*(A lhs, B rhs) {
    using unit_t = unit_make_from_sorted_t<
        decltype(lhs.get() * rhs.get()),
        scale_multiply_t<typename A::scale_factor, typename B::scale_factor>,
        merge_sum_sorted_t<typename A::dimensions, typename B::dimensions>>;

    return unit_t{lhs.get() * rhs.get()};
}

constexpr inline auto operator*(Unit lhs, Arithmetic rhs) {
    return lhs * unit{rhs};
}

constexpr inline auto operator*(Arithmetic lhs, Unit rhs) {
    return unit{lhs} * rhs;
}

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator/(unit<Tl, Sl, Dl...> lhs,
                                unit<Tr, Sr, Dr...> rhs) {
    using dimensions =
        merge_sum_sorted_t<list<Dl...>,
                           list<dimension_multiply_t<Dr, std::ratio<-1>>...>>;

    using unit_t = unit_make_from_sorted_t<decltype(lhs.get() / rhs.get()),
                                           scale_divide_t<Sl, Sr>, dimensions>;

    return unit_t{lhs.get() / rhs.get()};
}

constexpr inline auto operator/(Unit lhs, Arithmetic rhs) {
    return lhs / unit{rhs};
}

constexpr inline auto operator/(Arithmetic lhs, Unit rhs) {
    return unit{lhs} / rhs;
}

}  // namespace unit
