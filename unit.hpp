#pragma once

#include <cmath>
#include <cstdint>  // std::intmax_t
#include <ratio>
#include <type_traits>

// Attribution - This library uses (potentially) modified sections of code taken
// from the following sources.
//
// https://www.reddit.com/r/cpp/comments/bhxx49/c20_string_literals_as_nontype_template/
// https://stackoverflow.com/questions/6713420/c-convert-integer-to-string-at-compile-time

namespace unit {

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

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

// Convenience struct for inheriting: using type = ...
template <typename T = void>
struct Type {
    using type = T;
};

///////////////////////////// Static Strings ///////////////////////////////

namespace string {

template <std::size_t N>
class Static {
   private:
    char buf[N]{};  // null terminated char array

   public:
    constexpr Static(char const *s) {
        for (std::size_t i = 0; i < N; ++i) {
            buf[i] = s[i];
        }
    }

    Static() = default;
    Static(Static const &) = default;
    Static(Static &&) = default;

    Static &operator=(Static const &) = default;
    Static &operator=(Static &&) = default;

    static constexpr std::size_t size() { return N; }  // != length

    constexpr char operator[](std::size_t i) const { return buf[i]; }

    constexpr operator char const *() const { return buf; }  // implicit

    // string concatenation
    template <std::size_t O>
    constexpr auto operator+(Static<O> other) const {
        constexpr std::size_t len = size() + other.size() - 1;
        char concat[len]{};

        for (std::size_t i = 0; i < size() - 1; ++i) {  // miss null
            concat[i] = buf[i];
        }

        for (std::size_t i = 0; i < other.size(); ++i) {
            concat[size() - 1 + i] = other[i];
        }

        return Static<len>{concat};
    }
};

template <unsigned N>
Static(char const (&)[N])->Static<N>;

// Performs standard string comparison on static string types
template <std::size_t A, std::size_t B>
constexpr int compare(Static<A> a, Static<B> b) {
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

namespace detail {
// Compile time integer stringification

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
struct int_to_str_impl
    : int_to_str_impl<size - 1, x / 10, '0' + abs_val(x) % 10, args...> {};

// special case for two digits; minus sign is handled here
template <std::intmax_t x, char... args>
    struct int_to_str_impl<2, x, args...>
    : metastring < x<0 ? '-' : '0' + x / 10, '0' + abs_val(x) % 10, args...> {};

// end case for one digit (positive numbers only)
template <std::intmax_t x, char... args>
struct int_to_str_impl<1, x, args...> : metastring<'0' + x, args...> {};

}  // namespace detail

template <std::intmax_t x>
struct int_to_str {
    using type = detail::int_to_str_impl<detail::num_digits(x), x, '\0'>;
    static constexpr Static<type::size()> get() { return type::data; }
};

}  // namespace string

//////////////////////////////  SCALE  //////////////////////////////////

namespace detail {

struct scale_tag {};  // marks class as being of scale type

}  // namespace detail

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct ScaleBase : private detail::scale_tag {
    using significand = std::ratio<J, K>;

    static constexpr std::intmax_t exp = I;
    static constexpr std::intmax_t num = significand::num;
    static constexpr std::intmax_t den = significand::den;

    static_assert(num > 0, "Cannot have zero or negative scaled dimension");
};

template <typename T>
concept Scale = std::is_base_of_v<detail::scale_tag, T>;

// Using a variadic template instead of defaulted results in shorter types

template <std::intmax_t... Is>
struct scale {
    static_assert(sizeof...(Is) < 4, "Scale has too many template arguments");
};

template <>
struct scale<> : ScaleBase<0, 1, 1> {};

template <std::intmax_t I>
struct scale<I> : ScaleBase<I, 1, 1> {};

template <std::intmax_t I, std::intmax_t J>
struct scale<I, J> : ScaleBase<I, J, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale<I, J, K> : ScaleBase<I, J, K> {};

namespace detail {

enum : int { multiply, done, divide };

template <typename Ratio>
constexpr int standard_direction() {
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-10>>) {
        return divide;
    }
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-1>>) {
        return done;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<0>>) {
        return multiply;
    }
    if constexpr (std::ratio_equal_v<Ratio, std::ratio<0>>) {
        return done;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<1>>) {
        return multiply;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<10>>) {
        return done;
    }
    if constexpr (std::ratio_greater_equal_v<Ratio, std::ratio<10>>) {
        return divide;
    }
}

// forward declaration for match
template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl;

template <int C, std::intmax_t Exp, typename Ratio>
struct standard_match;

// end condition
template <std::intmax_t Exp, typename Ratio>
struct standard_match<done, Exp, Ratio> : Type<Ratio> {
    static constexpr std::intmax_t exp = Exp;
};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<divide, Exp, Ratio>
    : standard_form_impl<Exp + 1, std::ratio_divide<Ratio, std::ratio<10>>> {};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<multiply, Exp, Ratio>
    : standard_form_impl<Exp - 1, std::ratio_multiply<Ratio, std::ratio<10>>> {
};

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl
    : standard_match<standard_direction<Ratio>(), Exp, Ratio> {};

}  // namespace detail

// convert a std::ratio to standard form, returns a struct with ::type = the
// standard form ratio and ::exp the base 10 exponent.
template <typename Ratio>
using standard_form = detail::standard_form_impl<0, Ratio>;

namespace detail {

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_impl : Type<scale<I, J, K>> {};

template <std::intmax_t I, std::intmax_t J>
struct scale_make_impl<I, J, 1> : Type<scale<I, J>> {};

template <std::intmax_t I>
struct scale_make_impl<I, 1, 1> : Type<scale<I>> {};

template <>
struct scale_make_impl<0, 1, 1> : Type<scale<>> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_help {
    using standard = standard_form<std::ratio<J, K>>;
    using type = scale_make_impl<I + standard::exp, standard::type::num,
                                 standard::type::den>::type;
};

}  // namespace detail

// returns the most minimal possible scale type in standard form
template <std::intmax_t I = 0, std::intmax_t J = 1, std::intmax_t K = 1>
using scale_make = detail::scale_make_help<I, J, K>::type;

namespace detail {

template <Scale A, Scale B>
struct scale_multiply {
    using product =
        std::ratio_multiply<typename A::significand, typename B::significand>;

    using type = scale_make<A::exp + B::exp, product::num, product::den>;
};

template <Scale A, Scale B>
struct scale_divide {
    using product =
        std::ratio_divide<typename A::significand, typename B::significand>;

    using type = scale_make<A::exp - B::exp, product::num, product::den>;
};

}  // namespace detail

template <Scale A, Scale B>
using scale_multiply_t = detail::scale_multiply<A, B>::type;

template <Scale A, Scale B>
using scale_divide_t = detail::scale_divide<A, B>::type;

// Could be generalised to arbitrary scale<...> !
template <Scale From, Scale To, Arithmetic T>
inline constexpr auto scale_convert(T const x) {
    using conversion = scale_divide_t<From, To>;

    constexpr std::intmax_t num = conversion::num;
    constexpr std::intmax_t den = conversion::den;
    constexpr std::intmax_t exp = conversion::exp;

    // std::cout << "conversion " << num << ' ' << den << ' ' << exp <<
    // std::endl;

    // all this required to avoid floating point multiplication without
    // -ffast-math
    if constexpr (exp == 0) {
        if constexpr (num == 1 && den == 1) {
            return x;
        } else if constexpr (num != 1 && den == 1) {
            return x * num;
        } else if constexpr (num == 1 && den != 1) {
            return x / den;
        } else {
            return (x * num) / den;  // brackets ensure type propagation
        }
    } else {
        constexpr auto pow10 = std::pow(10, exp);

        if constexpr (num == 1 && den == 1) {
            return x * pow10;
        } else if constexpr (num != 1 && den == 1) {
            return (x * num) * pow10;
        } else if constexpr (num == 1 && den != 1) {
            return (x / den) * pow10;
        } else {
            return ((x * num) / den) * pow10;
        }
    }
}

//////////////////////////////   Dimension ////////////////////////////////////

namespace detail {

struct dimension_tag {};  // Marks class as being a dimension type.

}  // namespace detail

// Defaults required for variadic instantiation
template <string::Static Str, std::intmax_t I = 1, std::intmax_t J = 1>
struct DimensionBase : private detail::dimension_tag {
    using exp = std::ratio<I, J>;

    static constexpr std::intmax_t num = exp::num;
    static constexpr std::intmax_t den = exp::den;

    static constexpr string::Static symbol = Str;
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
          typename Ratio>
struct dimension_add<Dim<Il...>, Ratio> {
    using sum = std::ratio_add<typename Dim<Il...>::exp, Ratio>;
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

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          typename Ratio>
struct dimension_multiply<Dim<Il...>, Ratio> {
    using product = std::ratio_multiply<typename Dim<Il...>::exp, Ratio>;
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
struct ordered_impl;

template <>
struct ordered_impl<list<>> : std::true_type {};

template <Dimension D>
struct ordered_impl<list<D>> : std::true_type {};

template <Dimension First, Dimension Second, Dimension... Tail>
struct ordered_impl<list<First, Second, Tail...>> {
    static constexpr bool value =
        string::compare(First::symbol, Second::symbol) < 0 &&
        ordered_impl<list<Second, Tail...>>::value;
};

}  // namespace detail

// Checks if a list<...> of dimensions satisfies strict ordering,
// e.g. d_n < d_n+1 == true for all n.
template <List L>
inline constexpr bool ordered_v = detail::ordered_impl<L>::value;

// extracts symbol from dimension and returns symbol as a static string
// decorated with exponent info if required.
template <Dimension D>
inline constexpr auto anotate() {
    constexpr std::intmax_t num = D::num;
    constexpr std::intmax_t den = D::den;

    constexpr string::Static space = " ";

    if constexpr (num == 1 && den == 1) {
        return space + D::symbol;

    } else if constexpr (num != 1 && den == 1) {
        return space + D::symbol + string::Static{"^"} +
               string::int_to_str<num>::get();

    } else if constexpr (den != 1) {
        return space + D::symbol + string::Static{"^"} +
               string::int_to_str<num>::get() + string::Static{"/"} +
               string::int_to_str<den>::get();
    }
}

// stringifys a scale<> into a minimal " (a/b x 10^c)" like form.
template <Scale S>
inline constexpr auto anotate() {
    constexpr std::intmax_t num = S::num;
    constexpr std::intmax_t den = S::den;
    constexpr std::intmax_t exp = S::exp;

    constexpr string::Static space = " (";
    constexpr string::Static end = ")";

    if constexpr (exp == 0) {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return string::Static{""};
            } else {
                return space + string::int_to_str<num>::get() + end;
            }
        } else {
            return space + string::int_to_str<num>::get() +
                   string::Static{"/"} + string::int_to_str<den>::get() + end;
        }
    } else {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return space + string::Static{"10^"} +
                       string::int_to_str<exp>::get() + end;
            } else {
                return space + string::int_to_str<num>::get() +
                       string::Static{" x 10^"} +
                       string::int_to_str<exp>::get() + end;
            }
        } else {
            return space + string::int_to_str<num>::get() +
                   string::Static{"/"} + string::int_to_str<den>::get() +
                   string::Static{" x 10^"} + string::int_to_str<exp>::get() +
                   end;
        }
    }
}

// Concatenates static strings
inline constexpr auto join(auto... symbols) {
    if constexpr (sizeof...(symbols) == 1) {
        return (... + symbols) + string::Static{" dimensionless"};
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

// Overloading to accept unit<> which holds ::dimension = list<Dims...>
template <Unit A, Unit B>
inline constexpr bool dimension_equal_v<A, B> =
    detail::dimension_equal<typename A::dimensions,
                            typename B::dimensions>::value;

// Safe unit conversion but returns raw float, double, etc
template <Unit To, Unit From>
[[nodiscard]] constexpr auto raw_convert(
    From const x) requires dimension_equal_v<To, From> {
    return scale_convert<typename From::scale_factor,
                         typename To::scale_factor>(x.get());
}

// The libraries central type.
template <Arithmetic T, Scale S, Dimension... Dims>
class unit {
   public:
    using value_type = T;
    using scale_factor = S;
    using dimensions = list<Dims...>;

    static_assert((... && (Dims::num != 0)),
                  "Unit dimension exponents cannot be zero.");

    static_assert(ordered_v<dimensions>,
                  "Unit dimensions must satisfy strict ordering.");

    unit() = default;
    unit(unit const &) = default;
    unit(unit &&) = default;

    unit &operator=(unit const &) = default;
    unit &operator=(unit &&) = default;

    constexpr explicit unit(Arithmetic value) : m_value{value} {};

    template <Unit U>
    requires dimension_equal_v<unit, U> constexpr explicit unit(U const &other)
        : m_value{raw_convert<unit>(other)} {}

    template <Unit U>
    requires dimension_equal_v<unit, U> constexpr unit &operator=(
        U const &other) {
        m_value = raw_convert<unit>(other);
        return *this;
    }

#ifdef UNIT_SCALAR_IMPLICIT_CONVERSION
    // Implicit conversions are dangerous but useful for radian -> sine(x) etc.
    inline constexpr operator value_type() const requires S::num == 1 &&
        S::den == 1 && S::exp == 0 && sizeof...(Dims) == 0 {
        return m_value;
    }

#endif

    inline constexpr value_type get() const noexcept { return m_value; }

    static constexpr char const *symbol() noexcept { return m_symbol; }

   private:
    value_type m_value;

    // Symbol contains scale info and dimension symbols / exponents.
    static constexpr string::Static m_symbol =
        join(anotate<S>(), anotate<Dims>()...);
};

namespace detail {

template <Arithmetic, Scale, typename>
struct make_unit_from_sorted;

template <Arithmetic T, Scale S, Dimension... Dims>
struct make_unit_from_sorted<T, S, list<Dims...>> : Type<unit<T, S, Dims...>> {
};

}  // namespace detail

// Helper to make a unit from a dimension list<...>.
template <Arithmetic T, Scale S, List L>
using make_unit_from_sorted_t = detail::make_unit_from_sorted<T, S, L>::type;

// Dimension safe unit conversion -- arithmetic type of unit may not equal
// To::value_type e.g. decltype(return) != To in all cases.
template <Unit To, Unit From>
[[nodiscard]] constexpr auto convert(
    From const x) requires dimension_equal_v<To, From> {
    return make_unit_from_sorted_t<decltype(raw_convert<To>(x)),
                                   typename To::scale_factor,
                                   typename To::dimensions>{raw_convert<To>(x)};
}

// cout unit with symbol
std::ostream &operator<<(std::ostream &os, const Unit &obj) {
    return os << obj.get() << obj.symbol();
}

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
    : match<string::compare(A::symbol, B::symbol), L, list<A, As...>,
            list<B, Bs...>> {};

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

// Main way for users to make units. Makes a unit type by simplifying and
// sorting the dimensions and simplifying the scale.
template <Arithmetic T, Scale S, Dimension... Dims>
using make = make_unit_from_sorted_t<T, scale_make<S::exp, S::num, S::den>,
                                     sort_t<dimension_simplify_t<Dims>...>>;

// Secondary way for users to make units. Makes a unit type by rescaling an
// existing unit type.
template <Scale S, Unit U>
using make_scaled =
    make_unit_from_sorted_t<typename U::value_type,
                            scale_multiply_t<S, typename U::scale_factor>,
                            typename U::dimensions>;

///////////////////////////  operators ////////////////////////////

template <Unit A, Unit B>
constexpr inline auto operator+(A const lhs,
                                B const rhs) requires dimension_equal_v<A, B> {
    using unit_t =
        make_unit_from_sorted_t<decltype(lhs.get() + raw_convert<A>(rhs)),
                                typename A::scale_factor,
                                typename A::dimensions>;

    return unit_t{lhs.get() + raw_convert<A>(rhs)};
}

template <Unit A, Unit B>
constexpr inline auto operator-(A lhs, B rhs) requires dimension_equal_v<A, B> {
    using unit_t =
        make_unit_from_sorted_t<decltype(lhs.get() - raw_convert<A>(rhs)),
                                typename A::scale_factor,
                                typename A::dimensions>;

    return unit_t{lhs.get() - raw_convert<A>(rhs)};
}

template <Unit A, Unit B>
constexpr inline auto operator*(A lhs, B rhs) {
    using unit_t = make_unit_from_sorted_t<
        decltype(lhs.get() * rhs.get()),
        scale_multiply_t<typename A::scale_factor, typename B::scale_factor>,
        merge_sum_sorted_t<typename A::dimensions, typename B::dimensions>>;

    return unit_t{lhs.get() * rhs.get()};
}

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator/(unit<Tl, Sl, Dl...> lhs,
                                unit<Tr, Sr, Dr...> rhs) {
    using dimensions =
        merge_sum_sorted_t<list<Dl...>,
                           list<dimension_multiply_t<Dr, std::ratio<-1>>...>>;

    using unit_t = make_unit_from_sorted_t<decltype(lhs.get() / rhs.get()),
                                           scale_divide_t<Sl, Sr>, dimensions>;

    return unit_t{lhs.get() / rhs.get()};
}

}  // namespace unit

/////////////////////////// SI base units   ////////////////////////////

// it is undefined behaviour to: using namespace::si, so... don't ;)
namespace si {

// '_' prefix marks dimension rather than name-space to keep names short.

template <std::intmax_t... Is>
struct _meter : unit::DimensionBase<"m", Is...> {};

template <std::intmax_t... Is>
struct _second : unit::DimensionBase<"s", Is...> {};

template <std::intmax_t... Is>
struct _kilogram : unit::DimensionBase<"kg", Is...> {};

template <std::intmax_t... Is>
struct _ampere : unit::DimensionBase<"A", Is...> {};

template <std::intmax_t... Is>
struct _kelvin : unit::DimensionBase<"K", Is...> {};

template <std::intmax_t... Is>
struct _mole : unit::DimensionBase<"mol", Is...> {};

template <std::intmax_t... Is>
struct _candela : unit::DimensionBase<"cd", Is...> {};

// base units

template <typename T>
using meter = unit::unit<T, unit::scale<>, si::_meter<>>;

template <typename T>
using kilogram = unit::unit<T, unit::scale<>, si::_kilogram<>>;

// derived units

namespace prefix {

using nano = unit::scale<-9>;

}  // namespace prefix

//

// Using a variadic template instead of defaulted results in shorter types

}  // namespace si
