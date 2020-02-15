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

#include <cstddef>  // std::size_t
#include <cstdint>  // std::intmax_t
#include <ratio>
#include <type_traits>

#include "fixed_string.hpp"
#include "scale.hpp"  // Type<>

namespace su {

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

}

// Returns new dimension with same type but and new exponent equal to the
// product of std::ratio O and the argument dimensions' exponent.
template <typename D, typename Ratio>
using dimension_multiply_t = detail::dimension_multiply<D, Ratio>::type;

namespace detail {

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is,
          typename Ratio>
struct dimension_multiply<Dim<Is...>, Ratio> {
    using product = std::ratio_multiply<typename Dim<Is...>::exp, Ratio>;
    using type = dimension_simplify_t<Dim<product::num, product::den>>;
};

template <typename Ratio, Dimension... Dims>
struct dimension_multiply<list<Dims...>, Ratio>
    : Type<list<dimension_multiply_t<Dims, Ratio>...>> {};

}  // namespace detail

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
        return D::symbol + super<ito_fs<num>>;

    } else if constexpr (den != 1) {
        return D::symbol + "^{"_fs + ito_fs<num> + "/"_fs + ito_fs<den> +
               "}"_fs;
    }
}

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

}  // namespace su