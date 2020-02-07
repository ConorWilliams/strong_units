#include <cstdint>
#include <ratio>
#include <stdexcept>
#include <type_traits>

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

///////////////////////////// STR ///////////////////////////////

class str_const {
   private:
    const char *const p_;
    const std::size_t sz_;

   public:
    template <std::size_t N>
    constexpr str_const(const char (&a)[N]) : p_(a), sz_(N - 1) {}

    constexpr char operator[](std::size_t n) {
        return n < sz_ ? p_[n] : throw std::out_of_range("");
    }

    constexpr std::size_t size() { return sz_; }
};

constexpr int str_compare(str_const a, str_const b) {
    if (a.size() < b.size()) {
        return -1;
    } else if (b.size() > a.size()) {
        return 1;
    } else {
        for (std::size_t i = 0; i < a.size(); ++i) {
            if (a[i] < b[i]) {
                return -1;
            } else if (a[i] > b[i]) {
                return 1;
            }
        }

        return 0;
    }
}

///////////////////////////// SCALE //////////////////////////////////

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct ScaleBase {
    static constexpr std::intmax_t t_param[4] = {I, J, K, L};

    using fact = std::ratio<I, J>;
    using ofst = std::ratio<K, L>;

    static_assert(fact::num != 0, "Cannot have a zero scaled dimension");
    static_assert(ofst::den >= 0, "");  // use std::ratio msg
};

template <typename T>
concept Scale = std::is_base_of_v<
    ScaleBase<T::t_param[0], T::t_param[1], T::t_param[2], T::t_param[3]>, T>;

// Using a variadic template instead of defaulted results in shorter types

template <std::intmax_t... Is>
struct scale;

template <>
struct scale<> : ScaleBase<1, 1, 0, 1> {};

template <std::intmax_t I>
struct scale<I> : ScaleBase<I, 1, 0, 1> {};

template <std::intmax_t I, std::intmax_t J>
struct scale<I, J> : ScaleBase<I, J, 0, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale<I, J, K> : ScaleBase<I, J, K, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct scale<I, J, K, L> : ScaleBase<I, J, K, L> {};

namespace detail {

template <typename, typename>
struct scale_equal : std::false_type {};

template <std::intmax_t... Il, std::intmax_t... Ir>
struct scale_equal<scale<Il...>, scale<Ir...>> {
    static constexpr bool value =
        std::ratio_equal_v<typename scale<Il...>::fact,
                           typename scale<Ir...>::fact> &&

        std::ratio_equal_v<typename scale<Il...>::ofst,
                           typename scale<Ir...>::ofst>;
};

}  // namespace detail

template <Scale A, Scale B>
inline constexpr bool scale_equal_v = detail::scale_equal<A, B>::value;

// Could be generalised to arbitrary scale<...> !

template <Scale From, Scale To, Arithmetic T>
inline constexpr T convert(T x) {
    using fact = std::ratio_divide<typename From::fact, typename To::fact>;

    using ofst = std::ratio_divide<
        std::ratio_subtract<typename From::ofst, typename To::ofst>,
        typename To::fact>;

    if constexpr (std::ratio_equal_v<ofst, std::ratio<0>>) {
        if constexpr (std::ratio_equal_v<fact, std::ratio<0>>) {
            return x;
        } else {
            return static_cast<T>(fact::num) / fact::den * x;
        }
    } else {
        if constexpr (std::ratio_equal_v<fact, std::ratio<0>>) {
            return x + static_cast<T>(ofst::num) / ofst::den;
        } else {
            return static_cast<T>(fact::num) / fact::den * x +
                   static_cast<T>(ofst::num) / ofst::den;
        }
    }
}

//////////////////////////////   Dimension ////////////////////////////////////

template <std::intmax_t I, std::intmax_t J>
struct DimBaseBase {
    static constexpr std::intmax_t t_param[2] = {I, J};
    using exp = std::ratio<I, J>;
};

// Using a variadic template instead of defaulted results in shorter types

template <std::intmax_t... Is>
struct DimensionBase;

template <>
struct DimensionBase<> : DimBaseBase<1, 1> {};

template <std::intmax_t I>
struct DimensionBase<I> : DimBaseBase<I, 1> {};

template <std::intmax_t I, std::intmax_t J>
struct DimensionBase<I, J> : DimBaseBase<I, J> {};

template <typename T>
concept Dimension =
    std::is_base_of_v<DimBaseBase<T::t_param[0], T::t_param[1]>, T>;

// Test if two types are specialisations of the same dimension type

template <typename, typename>
struct same_dimension : std::false_type {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          std::intmax_t... Ir>
struct same_dimension<Dim<Il...>, Dim<Ir...>> {
    static constexpr bool value = true;
};

template <Dimension A, Dimension B>
struct equal_dimension {
    static constexpr bool value =
        same_dimension<A, B>::value &&
        std::ratio_equal_v<typename A::exp, typename B::exp>;
};

/////////////////////////// SI base units   ////////////////////////////

namespace si {

template <std::intmax_t... Is>
struct meter : DimensionBase<Is...> {
    static constexpr str_const symbol = "m";
};

template <std::intmax_t... Is>
struct second : DimensionBase<Is...> {
    static constexpr str_const symbol = "s";
};

template <std::intmax_t... Is>
struct kilogram : DimensionBase<Is...> {
    static constexpr str_const symbol = "kg";
};

template <std::intmax_t... Is>
struct ampere : DimensionBase<Is...> {
    static constexpr str_const symbol = "A";
};

}  // namespace si

//////////////////////////// UNIT  //////////////////////////////

template <Dimension First, Dimension Second, Dimension... Tail>
constexpr bool ordered_impl(First first, Second second, Tail... tail) {
    if constexpr (str_compare(first.symbol, second.symbol) < 0) {
        if constexpr (sizeof...(Tail) == 0) {
            return true;
        } else {
            return ordered(second, tail...);
        }
    } else {
        return false;
    }
}

// Verify that all Dims are ordered according to their symbol e.g
// s0 < s1 && s1 < s2 && ... && s(n-1) < s(n)
template <Dimension... Dims>
constexpr bool ordered(Dims... dims) {
    if constexpr (sizeof...(Dims) < 2) {
        return true;
    } else
        return ordered_impl(dims...);
}

// value_in_base = s::fact * value + s::ofst
template <Arithmetic T, Scale S, Dimension... Dims>
    requires ordered(Dims{}...) && (... && (Dims::exp::num != 0)) struct unit {
    using value_type = T;

    value_type value;

    inline constexpr value_type get() const { return value; }
};

template <typename>
struct is_unit : std::false_type {};

template <Arithmetic T, Scale S, Dimension... Dims>
struct is_unit<unit<T, S, Dims...>> : std::true_type {};

template <typename T>
concept Unit = is_unit<T>::value;

///////////////////////////  operators ////////////////////////////

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator+(
    unit<Tl, Sl, Dl...> lhs,
    unit<Tr, Sr, Dr...>
        rhs) requires std::conjunction_v<equal_dimension<Dl, Dr>...> {
    if constexpr (scale_equal_v<Sl, Sr>) {
        return unit<decltype(lhs.get() + rhs.get()), Sl, Dl...>{lhs.get() +
                                                                rhs.get()};
    } else {
        return unit<decltype(lhs.get() + rhs.get()), Sl, Dl...>{
            lhs.get() + convert<Sr, Sl>(rhs.get())};
    }
}

// lightweight list type required to separate dimension parameter packs in merge
template <Dimension... Dims>
struct list {
    static constexpr std::size_t size = sizeof...(Dims);
};

template <std::size_t, Dimension, typename>
struct find;

template <std::size_t N, Dimension Find>
struct find<N, Find, list<>> {
    static constexpr std::size_t value = N;
};

template <std::size_t N, Dimension Find, Dimension Head, Dimension... Tail>
struct find<N, Find, list<Head, Tail...>> {
   private:
    static constexpr bool found = same_dimension<Find, Head>::value;

   public:
    static constexpr std::size_t value =
        found ? N : find<N + 1, Find, list<Tail...>>::value;
};

template <typename, typename>
struct concat;

template <Dimension... Head, Dimension... Tail>
struct concat<list<Head...>, list<Tail...>> {
    using type = list<Head..., Tail...>;
};

template <typename A, typename B>
using concat_t = concat<A, B>::type;

namespace detail {

template <std::size_t N, typename List, Dimension Head, Dimension... Tail>
constexpr auto head(List l, list<Head, Tail...> tail) {
    if constexpr (List::size == N) {
        return l;
    } else if constexpr (List::size == N - 1) {
        return concat_t<List, list<Head>>{};
    } else {
        return head<N>(concat_t<List, list<Head>>{}, list<Tail...>{});
    }
}

}  // namespace detail

template <std::size_t N, typename List>
    requires N <=
    List::size using head = decltype(detail::head<N>(list<>{}, List{}));

// A B C D detail::head<N, list<>, List>::type
//     C D E F

// A B

//     C D

//     C D E F

// constexpr auto merge(list<> lhs, list<> rhs) { return list<>{}; }

// template <Dimension A, Dimension... As>
// constexpr auto merge(list<A, As...> lhs, list<> rhs) {
//     return lhs;
// }

// template <Dimension B, Dimension... Bs>
// constexpr auto merge(list<> lhs, list<B, Bs...> rhs) {
//     return rhs;
// }

// template <Dimension A, Dimension... As, Dimension B, Dimension...
// Bs> constexpr auto merge(list<A, As...> lhs, list<B, Bs...> rhs) {
//     using f1 = find<A, B, Bs...>;

//     if constexpr (f1::value) {
//     } else {
//         return typename concat<list<A>,
//                                decltype(merge(list<>{},
//                                list<>{}))>::type{};
//     }
// }

// merge(list<As...>{}, list<B, Bs...>)
