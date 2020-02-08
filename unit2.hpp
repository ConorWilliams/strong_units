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
    } else if (a.size() > b.size()) {
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

    using scale_factor = std::ratio<I, J>;
    using zero_offset = std::ratio<K, L>;

    static_assert(scale_factor::num != 0,
                  "Cannot have a zero scaled dimension");
    static_assert(zero_offset::den >= 0, "");  // use std::ratio msg
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

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct scale_simplify {
    using type = scale<I, J, K, L>;
};

template <>
struct scale_simplify<1, 1, 0, 1> {
    using type = scale<>;
};

template <std::intmax_t I>
struct scale_simplify<I, 1, 0, 1> {
    using type = scale<I>;
};

template <std::intmax_t I, std::intmax_t J>
struct scale_simplify<I, J, 0, 1> {
    using type = scale<I, J>;
};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_simplify<I, J, K, 1> {
    using type = scale<I, J, K>;
};

// namespace detail {

// template <typename, typename>
// struct scale_equal : std::false_type {};

// template <std::intmax_t... Il, std::intmax_t... Ir>
// struct scale_equal<scale<Il...>, scale<Ir...>> {
//     static constexpr bool value =
//         std::ratio_equal_v<typename scale<Il...>::scale_factor,
//                            typename scale<Ir...>::scale_factor> &&

//         std::ratio_equal_v<typename scale<Il...>::zero_offset,
//                            typename scale<Ir...>::zero_offset>;
// };

// }  // namespace detail

// template <Scale A, Scale B>
// inline constexpr bool scale_equal_v = detail::scale_equal<A, B>::value;

template <Scale A, Scale B>
concept zero_offset_equal =
    std::ratio_equal_v<typename A::zero_offset, typename B::zero_offset>;

// Could be generalised to arbitrary scale<...> !

template <Scale From, Scale To, Arithmetic T>
inline constexpr T convert(T x) {
    using scale_factor = std::ratio_divide<typename From::scale_factor,
                                           typename To::scale_factor>;

    using zero_offset =
        std::ratio_divide<std::ratio_subtract<typename From::zero_offset,
                                              typename To::zero_offset>,
                          typename To::scale_factor>;

    if constexpr (std::ratio_equal_v<zero_offset, std::ratio<0>>) {
        if constexpr (std::ratio_equal_v<scale_factor, std::ratio<0>>) {
            return x;
        } else {
            return static_cast<T>(scale_factor::num) / scale_factor::den * x;
        }
    } else {
        if constexpr (std::ratio_equal_v<scale_factor, std::ratio<0>>) {
            return x + static_cast<T>(zero_offset::num) / zero_offset::den;
        } else {
            return static_cast<T>(scale_factor::num) / scale_factor::den * x +
                   static_cast<T>(zero_offset::num) / zero_offset::den;
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

namespace detail {

template <typename, typename>
struct dimension_same : std::false_type {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          std::intmax_t... Ir>
struct dimension_same<Dim<Il...>, Dim<Ir...>> {
    static constexpr bool value = true;
};

}  // namespace detail

// Test if two types are specialisations of the same dimension type.
template <Dimension A, Dimension B>
inline constexpr bool dimension_same_v = detail::dimension_same<A, B>::value;

// Test if two types are specialisations of the same dimension type and have
// equal exponents.
template <Dimension A, Dimension B>
struct dimension_equal {
    static constexpr bool value =
        dimension_same_v<A, B> &&
        std::ratio_equal_v<typename A::exp, typename B::exp>;
};

// Returns new dimension with same type but and new exponent equal to sum of
// std::ratio O and old dimensions exponent.
template <typename, typename>
struct dimension_add;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          typename O>
struct dimension_add<Dim<Il...>, O> {
   private:
    using sum = std::ratio_add<typename Dim<Il...>::exp, O>;

   public:
    using type = std::conditional_t<sum::den == 1, Dim<sum::num>,
                                    Dim<sum::num, sum::den>>;
};

// Multiplies the exponents of two dimensions (does not assert they are the same
// type)
template <typename, typename>
struct dimension_multiply;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          typename O>
struct dimension_multiply<Dim<Il...>, O> {
   private:
    using product = std::ratio_multiply<typename Dim<Il...>::exp, O>;

   public:
    using type = std::conditional_t<product::den == 1, Dim<product::num>,
                                    Dim<product::num, product::den>>;
};

template <Dimension D, typename Ratio>
using dimension_multiply_t = dimension_multiply<D, Ratio>::type;

// template <typename>
// struct dimension_simplify;

// template <template <std::intmax_t...> typename Dim, std::intmax_t... Is>
// struct dimension_simplify<Dim<Is...>> {
//     using type = Dim<Dim<Is...>::exp::num, Dim<Is...>::exp::den>;
// };

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

// value_in_base = s::scale_factor * value + s::zero_offset
template <Arithmetic T, Scale S, Dimension... Dims>
    requires ordered(Dims{}...) && (... && (Dims::exp::num != 0)) class unit {
   public:
    using value_type = T;
    using scale_type = S;

    unit() = default;
    unit(unit const &) = default;
    unit(unit &&) = default;

    unit &operator=(unit const &) = default;
    unit &operator=(unit &&) = default;

    explicit unit(value_type value) : m_value{value} {};

    // template <Arithmetic OtherT, Scale OtherS, Dimension... OtherDims>
    // unit(unit<OtherT, OtherS, OtherDims...> other) : m_value{} {}

    // convert<OtherS, S>(other.get())
    // requires std::conjunction_v<dimension_equal<Dims, OtherDims>...>

    inline constexpr value_type get() const noexcept { return m_value; }

   private:
    value_type m_value;
};

template <typename>
struct is_unit : std::false_type {};

template <Arithmetic T, Scale S, Dimension... Dims>
struct is_unit<unit<T, S, Dims...>> : std::true_type {};

template <typename T>
concept Unit = is_unit<T>::value;

template <Unit To, Unit From>
auto convert(From x) {
    return convert<typename From::scale_type, typename To::scale_type>(x.get());
}

///////////////////////////  operators ////////////////////////////

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator+(unit<Tl, Sl, Dl...> lhs,
                                unit<Tr, Sr, Dr...> rhs) requires std::
    conjunction_v<dimension_equal<Dl, Dr>...> &&zero_offset_equal<Sl, Sr> {
    using value_type = decltype(lhs.get() + convert<Sr, Sl>(rhs.get()));

    return unit<value_type, Sl, Dl...>{lhs.get() + convert<Sr, Sl>(rhs.get())};
}

// lightweight list type required to separate dimension parameter packs
template <Dimension... Dims>
struct list {
    static constexpr std::size_t size = sizeof...(Dims);
};

namespace detail {

template <typename, typename>
struct concat_impl;

template <Dimension Head, Dimension... Tail>
struct concat_impl<Head, list<Tail...>> {
    using type = list<Head, Tail...>;
};

template <Dimension... Head, Dimension Tail>
struct concat_impl<list<Head...>, Tail> {
    using type = list<Head..., Tail>;
};

template <Dimension... Head, Dimension... Tail>
struct concat_impl<list<Head...>, list<Tail...>> {
    using type = list<Head..., Tail...>;
};

}  // namespace detail

// Concatenates two lists;
template <typename A, typename B>
using concat = detail::concat_impl<A, B>::type;

namespace detail {

template <typename List>
constexpr auto merge_sum_sorted_impl(list<> a, list<> b) {
    return List{};
}

template <typename List, Dimension A, Dimension... As>
constexpr auto merge_sum_sorted_impl(list<A, As...> a, list<> b) {
    return concat<List, list<A, As...>>{};
}

template <typename List, Dimension B, Dimension... Bs>
constexpr auto merge_sum_sorted_impl(list<> a, list<B, Bs...> b) {
    return concat<List, list<B, Bs...>>{};
}

template <typename List, Dimension A, Dimension... As, Dimension B,
          Dimension... Bs>
constexpr auto merge_sum_sorted_impl(list<A, As...> a, list<B, Bs...> b) {
    using a_t = list<A, As...>;
    using b_t = list<B, Bs...>;

    constexpr auto cmp = str_compare(A::symbol, B::symbol);

    if constexpr (cmp < 0) {
        return merge_sum_sorted_impl<concat<List, A>>(list<As...>{}, b);
    } else if constexpr (cmp > 0) {
        return merge_sum_sorted_impl<concat<List, B>>(a, list<Bs...>{});
    } else {
        static_assert(dimension_same_v<A, B>,
                      "During merge_sum found two conflicting dimensions with "
                      "the same symbol but different types.");

        using sum_t = dimension_add<A, typename B::exp>::type;

        if constexpr (std::ratio_equal_v<typename sum_t::exp, std::ratio<0>>) {
            return merge_sum_sorted_impl<List>(list<As...>{}, list<Bs...>{});
        } else {
            return merge_sum_sorted_impl<concat<List, sum_t>>(list<As...>{},
                                                              list<Bs...>{});
        }
    }
}

}  // namespace detail

// Merge two sorted lists of dimensions into a new sorted list summing any
// dimensions of the same type.
template <typename A, typename B>
using merge_sum_sorted =
    decltype(detail::merge_sum_sorted_impl<list<>>(A{}, B{}));

namespace detail {

template <Arithmetic, Scale, typename>
struct make_unit;

template <Arithmetic T, Scale S, Dimension... Dims>
struct make_unit<T, S, list<Dims...>> {
    using type = unit<T, S, Dims...>;
};

}  // namespace detail

template <Arithmetic T, Scale S, typename List>
using make_unit_t = detail::make_unit<T, S, List>::type;

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator*(
    unit<Tl, Sl, Dl...> lhs,
    unit<Tr, Sr, Dr...> rhs) requires zero_offset_equal<Sl, Sr> {
    using dimensions = merge_sum_sorted<list<Dl...>, list<Dr...>>;

    using product = std::ratio_multiply<typename Sl::scale_factor,
                                        typename Sr::scale_factor>;

    using scale_type =
        scale_simplify<product::num, product::den, Sl::zero_offset::num,
                       Sl::zero_offset::den>::type;

    using unit_t =
        make_unit_t<decltype(lhs.get() * rhs.get()), scale_type, dimensions>;

    return unit_t{lhs.get() * rhs.get()};
}

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator/(
    unit<Tl, Sl, Dl...> lhs,
    unit<Tr, Sr, Dr...> rhs) requires zero_offset_equal<Sl, Sr> {
    using dimensions =
        merge_sum_sorted<list<Dl...>,
                         list<dimension_multiply_t<Dr, std::ratio<-1>>...>>;

    using product =
        std::ratio_divide<typename Sl::scale_factor, typename Sr::scale_factor>;

    using scale_type =
        scale_simplify<product::num, product::den, Sl::zero_offset::num,
                       Sl::zero_offset::den>::type;

    using unit_t =
        make_unit_t<decltype(lhs.get() / rhs.get()), scale_type, dimensions>;

    return unit_t{lhs.get() / rhs.get()};
}
