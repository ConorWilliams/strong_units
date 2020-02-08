
namespace detail {

template <std::size_t, Dimension, typename>
struct find;

template <std::size_t N, Dimension Find>
struct find<N, Find, list<>> {
    static constexpr std::size_t value = N;
};

template <std::size_t N, Dimension Find, Dimension Head, Dimension... Tail>
struct find<N, Find, list<Head, Tail...>> {
   private:
    static constexpr bool found = dimension_same<Find, Head>::value;

   public:
    static constexpr std::size_t value =
        found ? N : find<N + 1, Find, list<Tail...>>::value;
};

}  // namespace detail

// Find the index of the Dimension matching 'Find' else return List::size
template <std::size_t N, Dimension Find, typename List>
inline constexpr bool find = detail::find<N, Find, List>::value;

namespace detail {

template <std::size_t N, typename List, Dimension Head, Dimension... Tail>
constexpr auto head_impl(List x, list<Head, Tail...> tail) {
    if constexpr (List::size == N) {
        return x;
    } else if constexpr (List::size == N - 1) {
        return concat<List, list<Head>>{};
    } else {
        return head<N>(concat<List, Head>{}, list<Tail...>{});
    }
}

}  // namespace detail

// Returns a list filled with the first N elements
template <std::size_t N, typename List>
    requires N <=
    List::size using head = decltype(detail::head_impl<N>(list<>{}, List{}));

namespace detail {

template <std::size_t N, Dimension Head, Dimension... Tail>
constexpr auto tail_impl(list<Head, Tail...> x) {
    constexpr auto len = sizeof...(Tail) + 1;

    if constexpr (N == len) {
        return x;
    } else if constexpr (N == len - 1) {
        return list<Tail...>{};
    } else {
        return tail<N>(list<Tail...>{});
    }
}

}  // namespace detail

// Returns a list filled with the last N elements
template <std::size_t N, typename List>
    requires N <=
    List::size using tail = decltype(detail::tail_impl<N>(List{}));
