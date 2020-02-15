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

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

namespace detail {
struct quantity_tag {};
}  // namespace detail

template <typename T>
concept Quantity = std::is_base_of_v<detail::quantity_tag, T>;

template <Unit U, Arithmetic Rep = double>
class quantity;

// The libraries central type.
template <Unit U, Arithmetic Rep>
class quantity : private detail::quantity_tag {
   public:
    using value_type = Rep;
    using unit = U;

    quantity() = default;
    quantity(quantity const&) = default;
    quantity(quantity&&) = default;

    quantity& operator=(quantity const&) = default;
    quantity& operator=(quantity&&) = default;

    constexpr quantity(Arithmetic value) : m_value{value} {};

    // Conversions occur hear
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

    inline constexpr quantity operator-() const { return quantity(-get()); }

    constexpr quantity& operator++() {
        ++m_value;
        return *this;
    }

    constexpr quantity operator++(int) { return quantity(m_value++); }

    constexpr quantity& operator--() {
        --m_value;
        return *this;
    }

    constexpr quantity operator--(int) { return quantity(m_value--); }

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
    requires dimension_equal_v<U, U2> friend constexpr auto operator<=>(
        quantity lhs, quantity<U2, Rep2> rhs) {
        return get() <=> raw_convert<U2, U>(other.get());
    }

#endif

    // template <Unit U2, Arithmetic Rep2>
    // requires dimension_equal_v<U, U2> friend constexpr auto operator==(
    //     quantity lhs, quantity<U2, Rep2> rhs) r {
    //     return cq(lhs).count() == cq(rhs).count();
    // }

    friend std::ostream& operator<<(std::ostream& os, const quantity obj) {
        return os << obj.get();
    }

   private:
    Rep m_value;

    // hidden operators
};

/////////////////////////////////  operators //////////////////////////////////

template <Unit U1, Arithmetic R1, Unit U2, Arithmetic R2>
constexpr inline auto operator+(
    quantity<U1, R1> const lhs,
    quantity<U2, R2> const rhs) requires dimension_equal_v<U1, U2> {
    // Using a common quantity and constructors to do the conversion to avoid
    // division and provide symmetry to + operation. Also throws warnings in
    // case of narrowing conversions in constructors;
    using common_s =
        common_scale<typename U1::scale_factor, typename U2::scale_factor>;

    // int i = typename U1::scale_factor{};

    // Bypass unit_make_t to prevent conversion to standard form of scale.
    using common_q =
        quantity<typename detail::unit_make_impl<true, common_s,
                                                 typename U1::dimensions>::type,
                 std::common_type_t<R1, R2>>;

    using rep = decltype(common_q(lhs).get() + common_q(rhs).get());

    // Now use unit_make_t to simplify scale.
    using quantity_t =
        quantity<downcast_unit<unit_make_t<common_s, typename U1::dimensions>>,
                 rep>;

    return quantity_t{common_q(lhs).get() + common_q(rhs).get()};
}

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator-(
//     quantity<Ul, Tl> const lhs,
//     quantity<Ur, Tr> const rhs) requires dimension_equal_v<Ul, Ur> {
//     //
//     using quantity_t =
//         quantity<Ul, decltype(lhs.get() - raw_convert<Ur, Ul>(rhs.get()))>;

//     return quantity_t{lhs.get() - raw_convert<Ur, Ul>(rhs.get())};
// }

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator*(quantity<Ul, Tl> const lhs,
//                                 quantity<Ur, Tr> const rhs) {
//     //
//     using unit_t = unit_make_from_sorted_t<
//         scale_multiply_t<typename Ul::scale_factor, typename
//         Ur::scale_factor>, merge_sum_sorted_t<typename Ul::dimensions,
//         typename Ur::dimensions>>;

//     using quanity_t =
//         quantity<downcast_unit<unit_t>, decltype(lhs.get() * rhs.get())>;

//     return quanity_t{lhs.get() * rhs.get()};
// }

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator/(quantity<Ul, Tl> const lhs,
//                                 quantity<Ur, Tr> const rhs) {
//     //
//     using dimensions = merge_sum_sorted_t<
//         typename Ul::dimensions,
//         dimension_multiply_t<typename Ur::dimensions, std::ratio<-1>>>;

//     using unit_t = unit_make_from_sorted_t<
//         scale_divide_t<typename Ul::scale_factor, typename Ur::scale_factor>,
//         dimensions>;

//     using quanity_t =
//         quantity<downcast_unit<unit_t>, decltype(lhs.get() / rhs.get())>;

//     return quanity_t{lhs.get() / rhs.get()};
// }

// constexpr inline auto operator*(Quantity lhs, Arithmetic rhs) {
//     return lhs * quantity<nameless<scale<>>, decltype(rhs)>{rhs};
// }

// constexpr inline auto operator*(Arithmetic lhs, Quantity rhs) {
//     return quantity<nameless<scale<>>, decltype(lhs)>{lhs} * rhs;
// }

// constexpr inline auto operator/(Quantity lhs, Arithmetic rhs) {
//     return lhs / quantity<nameless<scale<>>, decltype(rhs)>{rhs};
// }

// constexpr inline auto operator/(Arithmetic lhs, Quantity rhs) {
//     return quantity<nameless<scale<>>, decltype(lhs)>{lhs} / rhs;
// }

}  // namespace su
