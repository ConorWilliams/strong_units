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

#include <ostream>
#include <type_traits>

#include "dimension.hpp"
#include "fixed_string.hpp"
#include "scale.hpp"

namespace unit {

namespace detail {
struct unit_tag {};
}  // namespace detail

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

// Forward declaration for concept
template <Arithmetic T, Scale S, Dimension... Dims>
class unit;

// namespace detail {

// template <typename>
// struct is_unit : std::false_type {};

// template <Arithmetic T, Scale S, Dimension... Dims>
// struct is_unit<unit<T, S, Dims...>> : std::true_type {};

// }  // namespace detail

template <typename T>
concept Unit = std::is_base_of_v<detail::unit_tag, T>;

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

// Overloading to accept unit<> which holds ::dimension = list<Dims...>
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

//////////////////////////////////// UNIT ////////////////////////////////////

// The libraries central type.
template <Arithmetic T, Scale S, Dimension... Dims>
class unit : private detail::unit_tag {
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
    requires dimension_equal_v<unit, U> constexpr unit(U const &other)
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

/////////////////////////////////  operators //////////////////////////////////

std::ostream &operator<<(std::ostream &os, const Unit &obj) {
    return os << obj.get();
}

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
