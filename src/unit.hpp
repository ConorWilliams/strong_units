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

#include <type_traits>

#include "dimension.hpp"
#include "downcast.hpp"
#include "fixed_string.hpp"
#include "scale.hpp"

namespace su {

namespace detail {

struct unit_tag {};

}  // namespace detail

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

//////////////////////////////////// UNIT ////////////////////////////////////

template <Scale S, Dimension... Dims>
struct anon : downcast_base<anon<S, Dims...>>, detail::unit_tag {
   public:
    using scale_factor = S;
    using dimensions = list<Dims...>;

    static_assert((... && (Dims::num != 0)),
                  "Unit dimension exponents cannot be zero.");

    static_assert(ordered_v<dimensions>,
                  "Unit dimensions must satisfy strict ordering.");

    // Symbol contains scale info and dimension symbols / exponents.
    static constexpr fs::fixed_string m_base_symbol =
        join(anotate<S>(), anotate<Dims>()...);

    static constexpr fs::fixed_string m_symbol = m_base_symbol;
};

template <typename T>
concept Unit = std::is_base_of_v<detail::unit_tag, T>;

// inheritance injection to change symbol
template <auto Name, Unit U>
struct named_unit_base : U {
    static constexpr fs::fixed_string m_symbol = Name;
};

////////////////////////////////////////////////////////////////////////////////

// Overloading to accept unit<> which holds ::dimension = list<Dims...>
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

namespace detail {

template <Scale, typename>
struct unit_make_from_sorted;

template <Scale S, Dimension... Dims>
struct unit_make_from_sorted<S, list<Dims...>> : Type<anon<S, Dims...>> {};

}  // namespace detail

// Helper to make a unit from a dimension list<...>.
template <Scale S, List L>
using unit_make_from_sorted_t = detail::unit_make_from_sorted<S, L>::type;

// Makes a unit type by simplifying and sorting the dimensions and simplifying
// the scale.
template <Scale S, Dimension... Dims>
using unit_make_t =
    unit_make_from_sorted_t<scale_make<S::num, S::den, S::exp>,
                            sort_t<dimension_simplify_t<Dims>...>>;

template <typename Target, Scale S, Dimension... Dims>
struct unit : downcast_child<Target, unit_make_t<S, Dims...>> {};

template <typename Target, fs::fixed_string Sym, Scale S, Dimension... Dims>
struct named_unit
    : downcast_child<Target, named_unit_base<Sym, unit_make_t<S, Dims...>>> {};

// Safe unit conversion but returns raw float, double, etc
template <Unit To, Unit From>
[[nodiscard]] constexpr auto raw_convert(
    auto x) requires dimension_equal_v<To, From> {
    return scale_convert<typename From::scale_factor,
                         typename To::scale_factor>(x);
}

}  // namespace su
