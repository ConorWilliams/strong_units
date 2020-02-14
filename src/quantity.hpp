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

#include "unit.hpp"

namespace su {

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

namespace detail {
struct quantity_tag {};
}  // namespace detail

template <typename T>
concept Quantity = std::is_base_of_v<detail::quantity_tag, T>;

// The libraries central type.
template <Unit U, Arithmetic T = double>
class quantity : private detail::quantity_tag {
   public:
    using value_type = T;
    using unit = U;

    quantity() = default;
    quantity(quantity const &) = default;
    quantity(quantity &&) = default;

    quantity &operator=(quantity const &) = default;
    quantity &operator=(quantity &&) = default;

    constexpr explicit quantity(Arithmetic value) : m_value{value} {};

    template <Quantity U2>
    requires dimension_equal_v<U, U2> constexpr quantity(U2 const &other)
        : m_value{raw_convert<U2, U>(other.get())} {}

    template <Quantity U2>
    requires dimension_equal_v<U, U2> constexpr quantity &operator=(
        U2 const &other) {
        m_value = raw_convert<U2, U>(other.get());
        return *this;
    }

    [[nodiscard]] inline static constexpr std::string_view symbol() noexcept {
        return unit::m_symbol.view();
    }

    [[nodiscard]] inline static constexpr std::string_view
    base_symbol() noexcept {
        return unit::m_base_symbol.view();
    }

    [[nodiscard]] inline constexpr value_type get() const noexcept {
        return m_value;
    }

   private:
    value_type m_value;
};

/////////////////////////////////  operators //////////////////////////////////

std::ostream &operator<<(std::ostream &os, const Quantity &obj) {
    return os << obj.get();
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
constexpr inline auto operator+(
    quantity<Ul, Tl> const lhs,
    quantity<Ur, Tr> const rhs) requires dimension_equal_v<Ul, Ur> {
    //
    using quantity_t =
        quantity<Ul, decltype(lhs.get() + raw_convert<Ur, Ul>(rhs.get()))>;

    return quantity_t{lhs.get() + raw_convert<Ur, Ul>(rhs.get())};
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
constexpr inline auto operator-(
    quantity<Ul, Tl> const lhs,
    quantity<Ur, Tr> const rhs) requires dimension_equal_v<Ul, Ur> {
    //
    using quantity_t =
        quantity<Ul, decltype(lhs.get() - raw_convert<Ur, Ul>(rhs.get()))>;

    return quantity_t{lhs.get() - raw_convert<Ur, Ul>(rhs.get())};
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
constexpr inline auto operator*(quantity<Ul, Tl> const lhs,
                                quantity<Ur, Tr> const rhs) {
    //
    using unit_t = unit_make_from_sorted_t<
        scale_multiply_t<typename Ul::scale_factor, typename Ur::scale_factor>,
        merge_sum_sorted_t<typename Ul::dimensions, typename Ur::dimensions>>;

    using quanity_t =
        quantity<downcast<unit_t>, decltype(lhs.get() * rhs.get())>;

    return quanity_t{lhs.get() * rhs.get()};
}

constexpr inline auto operator*(Quantity lhs, Arithmetic rhs) {
    return lhs * quantity<anon<scale<>>, decltype(rhs)>{rhs};
}

constexpr inline auto operator*(Arithmetic lhs, Quantity rhs) {
    return quantity<anon<scale<>>, decltype(lhs)>{lhs} * rhs;
}

template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
constexpr inline auto operator/(quantity<Ul, Tl> const lhs,
                                quantity<Ur, Tr> const rhs) {
    //
    using dimensions = merge_sum_sorted_t<
        typename Ul::dimensions,
        dimension_multiply_t<typename Ur::dimensions, std::ratio<-1>>>;

    using unit_t = unit_make_from_sorted_t<
        scale_divide_t<typename Ul::scale_factor, typename Ur::scale_factor>,
        dimensions>;

    using quanity_t =
        quantity<downcast<unit_t>, decltype(lhs.get() / rhs.get())>;

    return quanity_t{lhs.get() / rhs.get()};
}

constexpr inline auto operator/(Quantity lhs, Arithmetic rhs) {
    return lhs / quantity<anon<scale<>>, decltype(rhs)>{rhs};
}

constexpr inline auto operator/(Arithmetic lhs, Quantity rhs) {
    return quantity<anon<scale<>>, decltype(lhs)>{lhs} / rhs;
}

}  // namespace su
