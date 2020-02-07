#pragma once

#include <cstdint>
#include <ratio>
#include <type_traits>

template <auto... Dims>
struct list {};

// namespace detail {

// template <std::size_t I, auto T>
// struct list_element;

// // recursive case
// template <std::size_t I, auto Head, auto... Tail>
// struct list_element<I, list<Head, Tail...>> {
//     static constexpr auto value = list_element<I - 1, list<Tail...>>::value
// };

// // base case
// template <auto Head, auto... Tail>
// struct list_element<0, list<Head, Tail...>> {
//     static constexpr auto value = Head;
// };

// }  // namespace detail

// template <std::size_t I, typename T>
// inline constexpr auto list_element = detail::list_element<I, T>::value;

#include <tuple>
#include <utility>

// swap types at index i and index j in the template argument tuple
template <std::size_t i, std::size_t j, typename List>
class swap {
    template <class Is>
    struct swap_impl;

    template <std::size_t... indices>
    struct swap_impl<std::index_sequence<indices...>> {
        using type = std::tuple<std::tuple_element_t<
            indices != i && indices != j ? indices : indices == i ? j : i,
            List>...>;
    };

   public:
    using type = typename swap_impl<
        std::make_index_sequence<std::tuple_size<List>::value>>::type;
};

// selection sort template argument tuple's variadic template's types
template <template <class, class> class Comparator, class Tuple>
class tuple_selection_sort {
    // selection sort's "loop"
    template <std::size_t i, std::size_t j, std::size_t tuple_size,
              class LoopTuple>
    struct tuple_selection_sort_impl {
        // this is done until we have compared every element in the type list
        using tuple_type = std::conditional_t<
            Comparator<std::tuple_element_t<i, LoopTuple>,
                       std::tuple_element_t<j, LoopTuple>>::value,
            typename swap<i, j, LoopTuple>::type,  // true:
                                                   // swap(i, j)
            LoopTuple                              // false: do nothing
            >;

        using type =
            typename tuple_selection_sort_impl  // recurse until j == tuple_size
            <i, j + 1, tuple_size, tuple_type   // using the modified tuple
             >::type;
    };

    template <std::size_t i, std::size_t tuple_size, class LoopTuple>
    struct tuple_selection_sort_impl<i, tuple_size, tuple_size, LoopTuple> {
        // once j == tuple_size, we increment i and start j at i + 1 and recurse
        using type =
            typename tuple_selection_sort_impl<i + 1, i + 2, tuple_size,
                                               LoopTuple>::type;
    };

    template <std::size_t j, std::size_t tuple_size, class LoopTuple>
    struct tuple_selection_sort_impl<tuple_size, j, tuple_size, LoopTuple> {
        // once i == tuple_size, we know that every element has been compared
        using type = LoopTuple;
    };

   public:
    using type =
        typename tuple_selection_sort_impl<0, 1, std::tuple_size<Tuple>::value,
                                           Tuple>::type;
};