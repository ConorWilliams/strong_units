// The MIT License (MIT)
//
// Copyright (c) 2020 Conor Williams
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <numeric>  // gcd / lcm
#include <ostream>
#include <type_traits>

#if __GNUC__ >= 10
#include <compare>
#endif

#include "math.hpp"
#include "unit.hpp"

namespace su {

using scalar_unit = nameless<scale<>>;

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

template <Unit U, Arithmetic Rep = double>
class quantity;

/////////////////////////////////////////////////////////////////////////////

template <Unit U1, typename R1, Unit U2, typename R2>
requires dimension_equal_v<U1, U2> struct common_help {
    // private:
    using S1 = U1::scale_factor;
    using S2 = U2::scale_factor;

    static constexpr std::array gcd =
        gcd_frac(S1::num, S1::den, S1::exp, S2::num, S2::den, S2::exp);

    // Scale factor that is the greatest common multiple of S1 and S2's
    // scale factors such that each scale is only scaled up in a conversion.
    // This avoids integer division where possible. Return scale is not in
    // standard form and therefore scale should not be used to make a quantity
    // without first passing through scale_make.
    using scale_t = scale<gcd[0], gcd[1], gcd[2]>;
    using dimension_t = U1::dimensions;  // == U2::dimension

    // Bypass unit_make_t to avoid scale conversion to standard form.
    using irreg_unit = detail::unit_make_impl<true, scale_t, dimension_t>::type;
    using irreg_quantity = quantity<irreg_unit, std::common_type_t<R1, R2>>;

   public:
    // Using a common quantity and constructors to do the conversion to avoid
    // division and provide symmetry to operations. Also throws warnings in
    // case of narrowing conversions in constructors.
    template <typename Quant>
    static constexpr auto conv(Quant x) {
        return irreg_quantity(x).get();
    }

    // Normalised common unit.
    using unit = downcast_unit<unit_make_t<scale_t, dimension_t>>;
};

///////////////////////////////////////////////////////////////////////////////

// The libraries central type.
template <Unit U, Arithmetic Rep>
class quantity {
   private:
    Rep m_value;

   public:
    using value_type = Rep;
    using unit = U;

    quantity() = default;
    quantity(quantity const&) = default;
    quantity(quantity&&) = default;

    quantity& operator=(quantity const&) = default;
    quantity& operator=(quantity&&) = default;

    constexpr explicit quantity(Arithmetic value) : m_value{value} {};

    // All conversions occur hear. Causes warnings if raw_convert type != Rep
    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> explicit constexpr quantity(
        quantity<U2, Rep2> other)
        : m_value{raw_convert<U2, U>(other.get())} {}

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator=(
        quantity<U2, Rep2> other) {
        m_value = quantity(other).get();
        return *this;
    }

    [[nodiscard]] static constexpr std::string_view symbol() noexcept {
        return unit::m_symbol.view();
    }

    [[nodiscard]] static constexpr std::string_view base_symbol() noexcept {
        return unit::m_base_symbol.view();
    }

    [[nodiscard]] constexpr Rep get() const noexcept { return m_value; }

    [[nodiscard]] inline constexpr quantity operator-() const {
        return quantity(-get());
    }

    [[nodiscard]] constexpr quantity operator+() const { return *this; }

    constexpr quantity& operator++() {
        ++m_value;
        return *this;
    }

    [[nodiscard]] constexpr quantity operator++(int) {
        return quantity(m_value++);
    }

    constexpr quantity& operator--() {
        --m_value;
        return *this;
    }

    [[nodiscard]] constexpr quantity operator--(int) {
        return quantity(m_value--);
    }

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator+=(
        quantity<U2, Rep2> other) {
        m_value += quantity(other).get();
        return *this;
    }

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator-=(
        quantity<U2, Rep2> other) {
        m_value -= quantity(other).get();
        return *this;
    }

    // modulo follow addition / subtraction rules e.g 4cm % 3cm = 1cm This is
    // due to the requirement of the existence of a scalar k such that:
    // k * 3cm + 1cm = 4cm
    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2>
        // && requires(Rep v1, Rep v2) {v1 %= v2;} // gcc bug?
        constexpr quantity& operator%=(quantity<U2, Rep2> other) {
        m_value %= quantity(other).get();
        return *this;
    }

    constexpr quantity& operator*=(Arithmetic rhs) {
        m_value *= rhs;
        return *this;
    }

    constexpr quantity& operator/=(Arithmetic rhs) {
        m_value /= rhs;
        return *this;
    }

   private:
    friend std::ostream& operator<<(std::ostream& os, quantity obj) {
        return os << obj.get() << ' ' << U::m_symbol;
    }
};

#if __GNUC__ >= 10

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator<=>(
    quantity<U1, Rep1> lhs,
    quantity<U2, Rep2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, Rep1, U2, Rep2>;
    return common::conv(lhs) <=> common::conv(rhs);
}

#endif

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator==(
    quantity<U1, Rep1> lhs,
    quantity<U2, Rep2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, Rep1, U2, Rep2>;
    return common::conv(lhs) == common::conv(rhs);
}

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator+(
    quantity<U1, Rep1> lhs,
    quantity<U2, Rep2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, Rep1, U2, Rep2>;
    using return_rep = decltype(common::conv(lhs) + common::conv(rhs));

    using quantity_t = quantity<typename common::unit, return_rep>;
    return quantity_t{common::conv(lhs) + common::conv(rhs)};
}

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator-(
    quantity<U1, Rep1> lhs,
    quantity<U2, Rep2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, Rep1, U2, Rep2>;
    using return_rep = decltype(common::conv(lhs) - common::conv(rhs));

    using quantity_t = quantity<typename common::unit, return_rep>;
    return quantity_t{common::conv(lhs) - common::conv(rhs)};
}

// clang-format off
template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator%(
    quantity<U1, Rep1> lhs,
    quantity<U2, Rep2> rhs) 

    requires dimension_equal_v<U1, U2> && 
    requires (std::common_type_t<Rep1, Rep2> x) {x % x;}
{
    using common = common_help<U1, Rep1, U2, Rep2>;
    using return_rep = decltype(common::conv(lhs) % common::conv(rhs));

    using quantity_t = quantity<typename common::unit, return_rep>;
    return quantity_t{common::conv(lhs) % common::conv(rhs)};
}
// clang-format on

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator*(quantity<U1, Rep1> lhs,
                                              quantity<U2, Rep2> rhs) {
    //
    using unit_t = unit_make_t<
        scale_multiply_t<typename U1::scale_factor, typename U2::scale_factor>,
        merge_sum_sorted_t<typename U1::dimensions, typename U2::dimensions>>;

    using quantity_t =
        quantity<downcast_unit<unit_t>, decltype(lhs.get() * rhs.get())>;

    return quantity_t{lhs.get() * rhs.get()};
}

template <Unit U1, Arithmetic Rep1, Unit U2, Arithmetic Rep2>
[[nodiscard]] inline constexpr auto operator/(quantity<U1, Rep1> lhs,
                                              quantity<U2, Rep2> rhs) {
    //
    using dimensions = merge_sum_sorted_t<
        typename U1::dimensions,
        dimension_multiply_t<typename U2::dimensions, std::ratio<-1>>>;

    using unit_t = unit_make_t<
        scale_divide_t<typename U1::scale_factor, typename U2::scale_factor>,
        dimensions>;

    using quanity_t =
        quantity<downcast_unit<unit_t>, decltype(lhs.get() / rhs.get())>;

    return quanity_t{lhs.get() / rhs.get()};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] inline constexpr auto operator*(quantity<U, Rep> lhs,
                                              Arithmetic rhs) {
    return lhs * quantity<scalar_unit, decltype(rhs)>{rhs};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] inline constexpr auto operator*(Arithmetic lhs,
                                              quantity<U, Rep> rhs) {
    return quantity<scalar_unit, decltype(lhs)>{lhs} * rhs;
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] inline constexpr auto operator/(quantity<U, Rep> lhs,
                                              Arithmetic rhs) {
    return lhs / quantity<scalar_unit, decltype(rhs)>{rhs};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] inline constexpr auto operator/(Arithmetic lhs,
                                              quantity<U, Rep> rhs) {
    return quantity<scalar_unit, decltype(lhs)>{lhs} / rhs;
}

/////////////////////////////////  operators //////////////////////////////////

}  // namespace su
