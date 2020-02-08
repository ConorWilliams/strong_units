#pragma once

#include <cstdint>  // std::intmax_t
#include <ratio>
#include <type_traits>

namespace sci {

// Lightweight list type required to separate parameter packs
template <typename... Ts>
struct list {
    static constexpr std::size_t size = sizeof...(Ts);
};

namespace detail {

template <typename>
struct is_list : std::false_type {};

template <typename... Ts>
struct is_list<list<Ts...>> : std::true_type {};

}  // namespace detail

template <typename T>
concept List = detail::is_list<T>::value;

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

// Convenience struct for inheriting: using type = ...
template <typename T = void>
struct Type {
    using type = T;
};

///////////////////////////// Static Strings ///////////////////////////////

template <std::size_t N>
class StaticString {
   private:
    char buf[N]{};  // null terminated char array

   public:
    constexpr StaticString(char const *s) {
        for (std::size_t i = 0; i < N; ++i) {
            buf[i] = s[i];
        }
    }

    StaticString() = default;
    StaticString(StaticString const &) = default;
    StaticString(StaticString &&) = default;

    StaticString &operator=(StaticString const &) = default;
    StaticString &operator=(StaticString &&) = default;

    static constexpr std::size_t size() { return N; }  // != length

    constexpr char operator[](std::size_t i) const { return buf[i]; }

    constexpr operator char const *() const { return buf; }  // implicit

    // string concatenation
    template <std::size_t O>
    constexpr auto operator+(StaticString<O> other) const {
        constexpr std::size_t len = size() + other.size() - 1;
        char concat[len]{};

        for (std::size_t i = 0; i < size() - 1; ++i) {  // miss null
            concat[i] = buf[i];
        }

        for (std::size_t i = 0; i < other.size(); ++i) {
            concat[size() - 1 + i] = other[i];
        }

        return StaticString<len>{concat};
    }
};

template <unsigned N>
StaticString(char const (&)[N])->StaticString<N>;

// Performs standard string comparison on static string types
template <std::size_t A, std::size_t B>
constexpr int str_compare(StaticString<A> a, StaticString<B> b) {
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

// Code to convert integer to char* lifted from John Lindgren's super answer:
//
// https://stackoverflow.com/questions/6713420/
// c-convert-integer-to-string-at-compile-time

constexpr std::intmax_t abs_val(std::intmax_t x) { return x < 0 ? -x : x; }

// calculate number of digits needed, including minus sign
constexpr std::intmax_t num_digits(std::intmax_t x) {
    return x < 0 ? 1 + num_digits(-x) : x < 10 ? 1 : 1 + num_digits(x / 10);
}

template <char... args>
struct metastring {
    static constexpr char data[sizeof...(args)] = {args...};
    static constexpr std::size_t size() { return sizeof...(args); }
};

template <std::intmax_t size, std::intmax_t x, char... args>
struct numeric_builder {
    using type = typename numeric_builder<size - 1, x / 10,
                                          '0' + abs_val(x) % 10, args...>::type;
};

template <std::intmax_t x, char... args>
struct numeric_builder<2, x, args...> {
    using type =
        metastring < x<0 ? '-' : '0' + x / 10, '0' + abs_val(x) % 10, args...>;
};

template <std::intmax_t x, char... args>
struct numeric_builder<1, x, args...> {
    using type = metastring<'0' + x, args...>;
};

template <std::intmax_t x>
struct itoa {
   private:
    using type = typename numeric_builder<num_digits(x), x, '\0'>::type;
    static constexpr type value{};

   public:
    static constexpr StaticString<type::size()> get() { return value.data; }
};

template <std::intmax_t x>
constexpr typename itoa<x>::type itoa<x>::value;

///////////////////////////// SCALE  //////////////////////////////////

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct ScaleBase {
    static constexpr std::intmax_t t_param[4] = {I, J, K, L};

    using scale_factor = std::ratio<I, J>;
    using zero_offset = std::ratio<K, L>;

    static_assert(zero_offset::den >= 0, "");  // triggers std::ratio _den != 0
    static_assert(scale_factor::num != 0,
                  "Cannot have a zero scaled dimension");
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

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct make_scale : Type<scale<I, J, K, L>> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct make_scale<I, J, K, 1> : Type<scale<I, J, K>> {};

template <std::intmax_t I, std::intmax_t J>
struct make_scale<I, J, 0, 1> : Type<scale<I, J>> {};

template <std::intmax_t I>
struct make_scale<I, 1, 0, 1> : Type<scale<I>> {};

template <>
struct make_scale<1, 1, 0, 1> : Type<scale<>> {};

}  // namespace detail

// returns the most minimal possible scale type
template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
using make_scale_t =
    detail::make_scale<std::ratio<I, J>::num, std::ratio<I, J>::den,
                       std::ratio<K, L>::num, std::ratio<K, L>::den>::type;

// currently only used once
template <Scale A, Scale B>
inline constexpr bool scale_equal_v =
    std::ratio_equal_v<typename A::scale_factor, typename B::scale_factor>
        &&std::ratio_equal_v<typename A::zero_offset, typename B::zero_offset>;

// Could be generalised to arbitrary scale<...> !

template <Scale From, Scale To, Arithmetic T>
inline constexpr T scale_convert(T x) {
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

template <StaticString Str, std::intmax_t I = 1, std::intmax_t J = 1>
struct DimensionBase {
    static constexpr StaticString symbol = Str;
    using exp = std::ratio<I, J>;
};

namespace detail {

template <typename>
struct is_dimension : std::false_type {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is>
struct is_dimension<Dim<Is...>> : std::true_type {};

}  // namespace detail

template <typename T>
concept Dimension = detail::is_dimension<T>::value &&requires(T) {
    typename T::exp;
    T::symbol;
};

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

template <typename>
struct dimension_simplify;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Is>
struct dimension_simplify<Dim<Is...>> {
    using exponent = Dim<Is...>::exp;
    using type = std::conditional_t<
        exponent::den == 1,
        std::conditional_t<exponent::num == 1, Dim<>, Dim<exponent::num>>,
        Dim<exponent::num, exponent::den>>;
};

}  // namespace detail

// Converts a dimensions into its shortest possible version representation
template <Dimension D>
using dimension_simplify_t = detail::dimension_simplify<D>::type;

namespace detail {
template <typename, typename>
struct dimension_add;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          typename O>
struct dimension_add<Dim<Il...>, O> {
    using sum = std::ratio_add<typename Dim<Il...>::exp, O>;
    using type = dimension_simplify_t<Dim<sum::num, sum::den>>;
};

}  // namespace detail

// Returns new dimension with same type but and new exponent equal to the sum of
// std::ratio O and argument dimensions' exponent.
template <Dimension D, typename Ratio>
using dimension_add_t = detail::dimension_add<D, Ratio>::type;

namespace detail {

template <typename, typename>
struct dimension_multiply;

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          typename O>
struct dimension_multiply<Dim<Il...>, O> {
    using product = std::ratio_multiply<typename Dim<Il...>::exp, O>;
    using type = dimension_simplify_t<Dim<product::num, product::den>>;
};

}  // namespace detail

// Returns new dimension with same type but and new exponent equal to the
// product of std::ratio O and the argument dimensions' exponent.
template <Dimension D, typename Ratio>
using dimension_multiply_t = detail::dimension_multiply<D, Ratio>::type;

//////////////////////////// UNIT  //////////////////////////////

namespace detail {

template <typename>
struct ordered;

template <>
struct ordered<list<>> : std::true_type {};

template <Dimension D>
struct ordered<list<D>> : std::true_type {};

template <Dimension First, Dimension Second, Dimension... Tail>
struct ordered<list<First, Second, Tail...>> {
    static constexpr bool value =
        str_compare(First::symbol, Second::symbol) < 0 &&
        ordered<list<Second, Tail...>>::value;
};

}  // namespace detail

template <List L>
inline constexpr bool ordered_v = detail::ordered<L>::value;

// extracts symbol from dimension and returns symbol as static string decorates
// decorated with exponent info if required.
template <Dimension D>
inline constexpr auto anotate() {
    constexpr std::intmax_t num = D::exp::num;
    constexpr std::intmax_t den = D::exp::den;

    constexpr StaticString space = " ";

    if constexpr (num == 1 && den == 1) {
        return space + D::symbol;

    } else if constexpr (num != 1 && den == 1) {
        return space + D::symbol + StaticString{"^"} + itoa<num>::get();

    } else if constexpr (den != 1) {
        return space + D::symbol + StaticString{"^"} + itoa<num>::get() +
               StaticString{"/"} + itoa<den>::get();
    }
}

// concatenates static strings
inline constexpr auto join(auto... symbols) {
    if constexpr (sizeof...(symbols) == 0) {
        return StaticString{" dimensionless"};
    } else {
        return (... + symbols);
    }
}

// forward declaration for concept
template <Arithmetic T, Scale S, Dimension... Dims>
class unit;

namespace detail {

template <typename>
struct is_unit : std::false_type {};

template <Arithmetic T, Scale S, Dimension... Dims>
struct is_unit<unit<T, S, Dims...>> : std::true_type {};

}  // namespace detail

template <typename T>
concept Unit = detail::is_unit<T>::value;

// Overloading
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

template <Unit A, Unit B>
inline constexpr bool zero_offset_equal_v =
    std::ratio_equal_v<typename A::scale_type::zero_offset,
                       typename B::scale_type::zero_offset>;

template <Unit To, Unit From>
[[nodiscard]] From::value_type raw_convert(
    From x) requires dimension_equal_v<To, From> {
    return scale_convert<typename From::scale_type, typename To::scale_type>(
        x.get());
}

template <Unit To, Unit From>
[[nodiscard]] To convert(From x) {
    return To{raw_convert<To>(x)};
}

// value_in_base = s::scale_factor * value + s::zero_offset
template <Arithmetic T, Scale S, Dimension... Dims>
class unit {
   public:
    using value_type = T;
    using scale_type = S;
    using dimensions = list<Dims...>;

    static_assert(dimensions::size || scale_equal_v<S, scale<>>,
                  "Dimensionless units cannot be scaled.");

    static_assert((... && (Dims::exp::num != 0)),
                  "Unit dimension exponents cannot be zero.");

    static_assert(ordered_v<dimensions>,
                  "Unit dimensions must satisfy strict ordering.");

    unit(unit const &) = default;
    unit(unit &&) = default;

    unit &operator=(unit const &) = default;
    unit &operator=(unit &&) = default;

    explicit unit(Arithmetic value) : m_value{value} {};

    unit(Unit const &other) : m_value{raw_convert<unit>(other)} {}

    unit &operator=(Unit const &other) {
        m_value = raw_convert<unit>(other);
        return *this;
    }

    inline constexpr value_type get() const noexcept { return m_value; }
    inline constexpr char const *symbol() const noexcept { return m_symbol; }

   private:
    value_type m_value;

    static constexpr StaticString m_symbol = join(anotate<Dims>()...);
};

namespace detail {

template <Arithmetic, Scale, typename>
struct make_unit_from_sorted;

template <Arithmetic T, Scale S, Dimension... Dims>
struct make_unit_from_sorted<T, S, list<Dims...>> : Type<unit<T, S, Dims...>> {
};

}  // namespace detail

template <Arithmetic T, Scale S, List L>
using make_unit_from_sorted_t = detail::make_unit_from_sorted<T, S, L>::type;

std::ostream &operator<<(std::ostream &os, const Unit &obj) {
    return os << obj.get() << obj.symbol();
}

///////////////////////////  operators ////////////////////////////

template <Unit A, Unit B>
constexpr inline A operator+(
    A lhs, B rhs) requires dimension_equal_v<A, B> &&zero_offset_equal_v<A, B> {
    return A{lhs.get() + raw_convert<A>(rhs)};
}

template <Unit A, Unit B>
constexpr inline A operator-(
    A lhs, B rhs) requires dimension_equal_v<A, B> &&zero_offset_equal_v<A, B> {
    return A{lhs.get() - raw_convert<A>(rhs)};
}

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

template <List L>
constexpr auto merge_sum_sorted(list<> a, list<> b) {
    return L{};
}

template <List L, Dimension A, Dimension... As>
constexpr auto merge_sum_sorted(list<A, As...> a, list<> b) {
    return concat_t<L, list<A, As...>>{};
}

template <List L, Dimension B, Dimension... Bs>
constexpr auto merge_sum_sorted(list<> a, list<B, Bs...> b) {
    return concat_t<L, list<B, Bs...>>{};
}

template <List L, Dimension A, Dimension... As, Dimension B, Dimension... Bs>
constexpr auto merge_sum_sorted(list<A, As...> a, list<B, Bs...> b) {
    using a_t = list<A, As...>;
    using b_t = list<B, Bs...>;

    constexpr auto cmp = str_compare(A::symbol, B::symbol);

    if constexpr (cmp < 0) {
        return merge_sum_sorted<concat_t<L, A>>(list<As...>{}, b);
    } else if constexpr (cmp > 0) {
        return merge_sum_sorted<concat_t<L, B>>(a, list<Bs...>{});
    } else {
        static_assert(dimension_same_v<A, B>,
                      "During merge_sum found two conflicting dimensions with "
                      "the same symbol but different types.");

        using sum_t = dimension_add_t<A, typename B::exp>;

        if constexpr (std::ratio_equal_v<typename sum_t::exp, std::ratio<0>>) {
            return merge_sum_sorted<L>(list<As...>{}, list<Bs...>{});
        } else {
            return merge_sum_sorted<concat_t<L, sum_t>>(list<As...>{},
                                                        list<Bs...>{});
        }
    }
}

}  // namespace detail

// Merge two sorted lists of dimensions into a new sorted list summing any
// dimensions of the same type.
template <List A, List B>
requires ordered_v<A> &&ordered_v<B> using merge_sum_sorted_t =
    decltype(detail::merge_sum_sorted<list<>>(A{}, B{}));

template <Unit A, Unit B>
constexpr inline auto operator*(A lhs,
                                B rhs) requires zero_offset_equal_v<A, B> {
    using product = std::ratio_multiply<typename A::scale_type::scale_factor,
                                        typename B::scale_type::scale_factor>;

    using scale_type = make_scale_t<product::num, product::den,
                                    A::scale_type::zero_offset::num,
                                    A::scale_type::zero_offset::den>;

    using unit_t = make_unit_from_sorted_t<
        decltype(lhs.get() * rhs.get()), scale_type,
        merge_sum_sorted_t<typename A::dimensions, typename B::dimensions>>;

    return unit_t{lhs.get() * rhs.get()};
}

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator/(
    unit<Tl, Sl, Dl...> lhs,
    unit<Tr, Sr, Dr...> rhs) requires zero_offset_equal_v<unit<Tl, Sl, Dl...>,
                                                          unit<Tr, Sr, Dr...>> {
    using dimensions =
        merge_sum_sorted_t<list<Dl...>,
                           list<dimension_multiply_t<Dr, std::ratio<-1>>...>>;

    using product =
        std::ratio_divide<typename Sl::scale_factor, typename Sr::scale_factor>;

    using scale_type = make_scale_t<product::num, product::den,
                                    Sl::zero_offset::num, Sl::zero_offset::den>;

    using unit_t = make_unit_from_sorted_t<decltype(lhs.get() / rhs.get()),
                                           scale_type, dimensions>;

    return unit_t{lhs.get() / rhs.get()};
}

////////////////////  SORTING   ///////////////////////

namespace detail {

template <List...>
struct sort_impl {};

template <>
struct sort_impl<> : Type<list<>> {};

template <List Single>
struct sort_impl<Single> : Type<Single> {};

template <List First, List Second, List... Tail>
struct sort_impl<First, Second, Tail...> {
    using type = sort_impl<merge_sum_sorted_t<First, Second>, Tail...>::type;
};

}  // namespace detail

// compile time merge sort a parameter pack of dimensions
template <Dimension... Dims>
using sort_t = detail::sort_impl<list<Dims>...>::type;

// makes a unit type by simplifying and sorting the dimensions and simplifying
// the scale
template <Arithmetic T, Scale S, Dimension... Dims>
using new_unit = make_unit_from_sorted_t<
    T,
    make_scale_t<S::scale_factor::num, S::scale_factor::den,
                 S::zero_offset::num, S::zero_offset::den>,
    sort_t<dimension_simplify_t<Dims>...>>;

}  // namespace sci

/////////////////////////// SI base units   ////////////////////////////

// exported names
using sci::scale;

using sci::Unit;

template <typename T, typename S, typename... Dims>
using unit = sci::new_unit<T, S, Dims...>;

using sci::convert;

namespace si {

// Using a variadic template instead of defaulted results in shorter types

template <std::intmax_t... Is>
struct meter : sci::DimensionBase<"m", Is...> {};

template <std::intmax_t... Is>
struct second : sci::DimensionBase<"s", Is...> {};

template <std::intmax_t... Is>
struct kilogram : sci::DimensionBase<"kg", Is...> {};

template <std::intmax_t... Is>
struct ampere : sci::DimensionBase<"A", Is...> {};

template <std::intmax_t... Is>
struct kelvin : sci::DimensionBase<"K", Is...> {};

template <std::intmax_t... Is>
struct mole : sci::DimensionBase<"mol", Is...> {};

template <std::intmax_t... Is>
struct candela : sci::DimensionBase<"cd", Is...> {};

}  // namespace si

namespace units {
using meter =
}
