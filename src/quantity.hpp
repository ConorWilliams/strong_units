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

#include <ostream>
#include <type_traits>

#if __GNUC__ >= 10
#include <compare>
#endif

#include "unit.hpp"

namespace su {

using scalar_unit = nameless<scale<>>;

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

template <Unit U, Arithmetic Rep = double>
class quantity;

/////////////////////////////////////////////////////////////////////////////

namespace detail {
// Returns the scale factor that is the greatest common multiple of S1 and S2's
// scale factors such that each scale is only scaled up in a conversion. This
// avoids integer division where possible. Return scale is not in standard form
// and therefore scale should used should not be used to make a quantity without
// first passing through scale_make
template <Scale S1, Scale S2>
struct common_scale {
    static constexpr std::intmax_t gcd_num = std::gcd(S1::num, S2::num);
    static constexpr std::intmax_t gcd_den = std::gcd(S1::den, S2::den);

    // Deliberate no use of make_scale to avoid standard form conversion
    using type = scale<gcd_num, (S1::den / gcd_den) * S2::den,
                       S1::exp <= S2::exp ? S1::exp : S2::exp>;
};

// Short-cut for same scales
template <Scale S>
struct common_scale<S, S> : Type<S> {};

}  // namespace detail

// Utility struct to make an non-normalised quantity that two units can be
// safely converted to and perform conversion.
template <Unit U1, typename R1, Unit U2, typename R2>
requires dimension_equal_v<U1, U2> struct common_help {
   private:
    using S1 = U1::scale_factor;
    using S2 = U2::scale_factor;

   public:
    using scale = detail::common_scale<S1, S2>::type;
    using dimension = U1::dimensions;  // == U2::dimension

   private:
    // Using a common quantity and constructors to do the conversion to avoid
    // division and provide symmetry to operations. Also throws warnings in
    // case of narrowing conversions in constructors. Bypass unit_make_t to
    // avoid scale conversion to standard form.
    //
    using unit_t = detail::unit_make_impl<true, scale, dimension>::type;
    using irregular_quantity = quantity<unit_t, std::common_type_t<R1, R2>>;

   public:
    template <typename Quant>
    static constexpr auto conv(Quant x) {
        return irregular_quantity(x).get();
    }

    using unit = downcast_unit<unit_make_t<scale, dimension>>;
};

///////////////////////////////////////////////////////////////////////////////

// The libraries central type.
template <Unit U, Arithmetic Rep>
class quantity {
   public:
    using value_type = Rep;
    using unit = U;

    quantity() = default;
    quantity(quantity const&) = default;
    quantity(quantity&&) = default;

    quantity& operator=(quantity const&) = default;
    quantity& operator=(quantity&&) = default;

    constexpr quantity(Arithmetic value) : m_value{value} {};

    // All conversions occur hear. Causes warnings if raw_convert type != Rep
    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity(
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

    constexpr quantity& operator*=(Arithmetic rhs) {
        m_value *= rhs;
        return *this;
    }

    constexpr quantity& operator/=(Arithmetic rhs) {
        m_value /= rhs;
        return *this;
    }

#if __GNUC__ >= 10

    template <Unit U2, Arithmetic Rep2>
    [[nodiscard]] friend constexpr auto operator<=>(
        quantity lhs,
        quantity<U2, Rep2> rhs) requires dimension_equal_v<U, U2> {
        using common = common_help<U, Rep, U2, Rep2>;
        return common::conv(lhs) <=> common::conv(rhs);
    }

#endif

    template <Unit U2, Arithmetic Rep2>
    [[nodiscard]] friend constexpr auto operator==(
        quantity lhs,
        quantity<U2, Rep2> rhs) requires dimension_equal_v<U, U2> {
        using common = common_help<U, Rep, U2, Rep2>;
        return common::conv(lhs) == common::conv(rhs);
    }

    friend std::ostream& operator<<(std::ostream& os, quantity obj) {
        return os << obj.get() << ' ' << U::m_symbol;
    }

   private:
    Rep m_value;

    // hidden operators
};

/////////////////////////////////  operators //////////////////////////////////

template <Unit U1, Arithmetic R1, Unit U2, Arithmetic R2>
[[nodiscard]] constexpr inline auto operator+(
    quantity<U1, R1> lhs,
    quantity<U2, R2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, R1, U2, R2>;

    using rep = decltype(common::conv(lhs) + common::conv(rhs));

    using quantity_t = quantity<typename common::unit, rep>;

    return quantity_t{common::conv(lhs) + common::conv(rhs)};
}

template <Unit U1, Arithmetic R1, Unit U2, Arithmetic R2>
[[nodiscard]] constexpr inline auto operator-(
    quantity<U1, R1> lhs,
    quantity<U2, R2> rhs) requires dimension_equal_v<U1, U2> {
    //
    using common = common_help<U1, R1, U2, R2>;

    using rep = decltype(common::conv(lhs) - common::conv(rhs));

    using quantity_t = quantity<typename common::unit, rep>;

    return quantity_t{common::conv(lhs) - common::conv(rhs)};
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
[[nodiscard]] constexpr inline auto operator*(quantity<Ul, Tl> lhs,
                                              quantity<Ur, Tr> rhs) {
    //
    using unit_t = unit_make_t<
        scale_multiply_t<typename Ul::scale_factor, typename Ur::scale_factor>,
        merge_sum_sorted_t<typename Ul::dimensions, typename Ur::dimensions>>;

    using quanity_t =
        quantity<downcast_unit<unit_t>, decltype(lhs.get() * rhs.get())>;

    return quanity_t{lhs.get() * rhs.get()};
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
[[nodiscard]] constexpr inline auto operator/(quantity<Ul, Tl> lhs,
                                              quantity<Ur, Tr> rhs) {
    //
    using dimensions = merge_sum_sorted_t<
        typename Ul::dimensions,
        dimension_multiply_t<typename Ur::dimensions, std::ratio<-1>>>;

    using unit_t = unit_make_t<
        scale_divide_t<typename Ul::scale_factor, typename Ur::scale_factor>,
        dimensions>;

    using quanity_t =
        quantity<downcast_unit<unit_t>, decltype(lhs.get() / rhs.get())>;

    return quanity_t{lhs.get() / rhs.get()};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] constexpr inline auto operator*(quantity<U, Rep> lhs,
                                              Arithmetic rhs) {
    return lhs * quantity<scalar_unit, decltype(rhs)>{rhs};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] constexpr inline auto operator*(Arithmetic lhs,
                                              quantity<U, Rep> rhs) {
    return quantity<scalar_unit, decltype(lhs)>{lhs} * rhs;
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] constexpr inline auto operator/(quantity<U, Rep> lhs,
                                              Arithmetic rhs) {
    return lhs / quantity<scalar_unit, decltype(rhs)>{rhs};
}

template <Unit U, Arithmetic Rep>
[[nodiscard]] constexpr inline auto operator/(Arithmetic lhs,
                                              quantity<U, Rep> rhs) {
    return quantity<scalar_unit, decltype(lhs)>{lhs} / rhs;
}

}  // namespace su
