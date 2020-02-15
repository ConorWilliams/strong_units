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

// Join annotated scale and dimensions with correct spaces.
template <typename Scale>
inline constexpr auto join(Scale scale) {
    // for string literal operator
    using namespace fs;

    if constexpr (Scale::size() == 0) {
        return "dimensionless"_fs;
    } else {
        return scale + " dimensionless"_fs;
    }
}

template <typename Scale, typename Head, typename... Tail>
inline constexpr auto join(Scale scale, Head head, Tail... tail) {
    // for string literal operator
    using namespace fs;

    if constexpr (Scale::size() == 0) {
        return fixed_string{head, ("\u22C5"_fs + tail)...};
    } else {
        return fixed_string{scale, " "_fs, head, ("\u22C5"_fs + tail)...};
    }
}

struct unit_tag {};  // marks unit for concept

}  // namespace detail

// Base class representing a general unit.
template <Scale S, Dimension... Dims>
struct nameless : downcast_base<nameless<S, Dims...>>, detail::unit_tag {
   public:
    using scale_factor = S;
    using dimensions = list<Dims...>;

    static_assert((... && (Dims::num != 0)),
                  "Unit dimension exponents cannot be zero.");

    static_assert(ordered_v<dimensions>,
                  "Unit dimensions must satisfy strict ordering.");

    // Symbol contains scale info and dimension symbols / exponents.
    static constexpr fs::fixed_string m_base_symbol =
        detail::join(anotate<S>(), anotate<Dims>()...);

    static constexpr fs::fixed_string m_symbol = m_base_symbol;
};

template <typename T>
concept Unit = std::is_base_of_v<detail::unit_tag, T>;

// Units which have scale<1, 1, 0> are coherent and therefore we can exclude
// that from the template parameter to shorten the quantity definition.
template <Dimension... Dims>
struct coherent : nameless<scale<>> {};

namespace detail {

// General case use downcast
template <Unit U, Unit D>
struct downcast_unit_impl : Type<D> {};

// Case for no downcast but unit is coherent
template <Unit U, Dimension... Dims>
struct downcast_unit_impl<U, nameless<scale<>, Dims...>>
    : Type<coherent<Dims...>> {};

// General case for no downcast
template <Unit U>
requires(!std::is_same_v<typename U::scale_factor,
                         scale<>>) struct downcast_unit_impl<U, U> : Type<U> {};

}  // namespace detail

// Downcast a unit to either a user defined unit a 'coherent' unit or a
// non-coherent 'nameless' unit
template <Unit U>
using downcast_unit = detail::downcast_unit_impl<U, downcast<U>>::type;

namespace detail {
// inheritance injection to change symbol when using named_unit<...> helper
template <auto Name, Unit U>
struct named_unit : U {
    static constexpr fs::fixed_string m_symbol = Name;
};

template <bool, Scale, List>
struct unit_make_impl;

// sorted case
template <Scale S, Dimension... Dims>
struct unit_make_impl<true, S, list<Dims...>> : Type<nameless<S, Dims...>> {};

// unsorted case
template <Scale S, Dimension... Dims>
struct unit_make_impl<false, S, list<Dims...>>
    : unit_make_impl<true, S, sort_t<Dims...>> {};

}  // namespace detail

// Makes a unit type from a dimension list<...> by simplifying and sorting the
// it and simplifying the scale.
template <Scale S, List L>
using unit_make_t =
    detail::unit_make_impl<ordered_v<L>, scale_make<S::num, S::den, S::exp>,
                           L>::type;

// *****************************************************************************
// *                  User access points for making new units                  *
// *****************************************************************************

template <typename Target, Scale S, Dimension... Dims>
struct unit : downcast_child<Target, unit_make_t<S, list<Dims...>>> {};

template <typename Target, fs::fixed_string Sym, Scale S, Dimension... Dims>
struct named_unit
    : downcast_child<Target,
                     detail::named_unit<Sym, unit_make_t<S, list<Dims...>>>> {};

////////////////////////////////////////////////////////////////////////////////

// Overloading to accept unit<> which holds ::dimension = list<Dims...>
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

// Safe unit conversion but returns raw float, double, etc
template <Unit From, Unit To>
[[nodiscard]] constexpr auto raw_convert(
    auto x) requires dimension_equal_v<From, To> {
    return scale_convert<typename From::scale_factor,
                         typename To::scale_factor>(x);
}

}  // namespace su
