// The MIT License (MIT)
//
// Copyright (c) 2018 Mateusz Pusz
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
//
//
// This file is taken from: https://github.com/mpusz/units
//
// Full credit to Mateusz Pusz for this fantastic idiom!
//
// Implements the (very clever) downcast template-idiom enabling (where =>
// denotes inheritance):
//
// struct parent => unit<i, j, k> => downcast_base<unit<i, j, k>>
//
// downcast(unit<i, j, k>) -> parent
//
// This is achieved by parent inheriting unit<...> through downcast_child which
// specialises a friend function defined in downcast base. downcast_child has
// full knowledge of parent through the CRTP idiom and can specialise the friend
// function in downcast base to return it. Full inheritance:
//
// parent => make_unit_helper<parent ...> => downcast_child<parent, ...> =>
// unit<...> => downcast_base<unit<...>>

#include <type_traits>

namespace su {

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnon-template-friend"

template <typename BaseType>
struct downcast_base {
    using downcast_base_type = BaseType;
    friend auto downcast_guide(downcast_base);
};

#pragma GCC diagnostic pop

template <typename T>
concept Downcastable = requires {
    typename T::downcast_base_type;
}
&&std::is_base_of_v<downcast_base<typename T::downcast_base_type>, T>;

template <typename Target, Downcastable T>
struct downcast_child : T {
    friend auto downcast_guide(typename downcast_child::downcast_base) {
        return Target();
    }
};

namespace detail {

template <typename T>
concept has_downcast = requires {
    downcast_guide(std::declval<downcast_base<T>>());
};

template <typename T>
constexpr auto downcast_impl() {
    if constexpr (has_downcast<T>) {
        return decltype(downcast_guide(std::declval<downcast_base<T>>()))();
    } else {
        return T();
    }
}

}  // namespace detail

template <Downcastable T>
using downcast = decltype(detail::downcast_impl<T>());

}  // namespace su
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

#include <array>
#include <cstddef>  // std::size_t
#include <cstdint>  // std::intmax_t
#include <ostream>
#include <string_view>
#include <utility>  // index_seq

#if __GNUC__ >= 10
#include <compare>
#endif

// Provides utilities for working with compile time strings and passing string
// literals as template parameters. See fixed_string.test.cpp for examples.

namespace fs {

// The fixed_string class is a literal class, implicitly constructable from
// char* and can therefore used to pass strings to templates. It publicly
// inherits from std::array and therefore inherits all its iterating goodness!
//
// Deduction guides are provided such that the template parameter N should
// almost never have to be explicitly provided, as well as a user defined
// literal allowing for the short syntax "string"_fs.
template <std::size_t N>
struct fixed_string : std::array<char, N> {
    //
    using std::array<char, N>::data;
    using std::array<char, N>::operator[];

    fixed_string& operator=(fixed_string&&) = default;
    fixed_string& operator=(fixed_string const&) = default;

#if __GNUC__ >= 10

    auto operator<=>(const string&) const = default;

#endif

    constexpr fixed_string(char c) : std::array<char, N>{c} {}

    constexpr fixed_string(char const* str) {
        for (std::size_t i = 0; i < N; ++i) operator[](i) = str[i];
    }

    constexpr fixed_string() : std::array<char, N>{} {};

    constexpr fixed_string(fixed_string const&) = default;

    // Construct by concentrating two other fixed_strings
    template <std::size_t I, std::size_t J>
    constexpr fixed_string(fixed_string<I> const& lhs,
                           fixed_string<J> const& rhs) {
        for (std::size_t i = 0; i < I; ++i) operator[](i) = lhs[i];
        for (std::size_t i = 0; i < J; ++i) operator[](i + I) = rhs[i];
    }

    // Construct by concentrating any number of fixed_strings
    template <std::size_t... Is>
    constexpr fixed_string(fixed_string<Is> const&... args) requires(
        sizeof...(args) > 2) {
        *this = (... + args);
    }

    // Get a view of a sub-string.
    inline constexpr std::string_view view(std::size_t start = 0,
                                           int end = N) const noexcept {
        return {data() + start, end - start};
    }

    inline static constexpr std::size_t size() { return N; }
};

template <std::size_t N>
fixed_string(char const (&)[N])->fixed_string<N - 1>;

fixed_string()->fixed_string<0>;

fixed_string(char)->fixed_string<1>;

template <std::size_t... Is>
fixed_string(fixed_string<Is>...)->fixed_string<(... + Is)>;

// C++20 string literal operator template : "abc"_fs -> fixed_string{"abc"}
template <fixed_string str>
inline constexpr auto operator"" _fs() {
    return str;
}

// + operator used for fixed_string concatenation
template <std::size_t I, std::size_t J>
inline constexpr auto operator+(fixed_string<I> const& lhs,
                                fixed_string<J> const& rhs) {
    return fixed_string{lhs, rhs};
}

template <std::size_t N>
std::ostream& operator<<(std::ostream& os, const fixed_string<N>& str) {
    return os << str.view();
}

// Standard c-like string comparison.
template <std::size_t I, std::size_t J>
constexpr int compare(fixed_string<I> lhs, fixed_string<J> rhs) {
    if constexpr (I < J) {
        return -1;
    }
    if constexpr (I > J) {
        return 1;
    }
    for (std::size_t i = 0; i < I; ++i) {
        if (lhs[i] < rhs[i]) {
            return -1;
        }
        if (lhs[i] > rhs[i]) {
            return 1;
        }
    }
    return 0;
}

namespace detail {

template <int base>
    requires base > 1 constexpr std::size_t num_digits(std::intmax_t x) {
    return x < 0 ? 1 + num_digits<base>(-x)
                 : x < base ? 1 : 1 + num_digits<base>(x / base);
}

inline constexpr fixed_string digits = {
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"};

template <std::intmax_t integer, int base>
    requires(base > 1) && (base < digits.size() + 1) constexpr auto ito_fs() {
    //
    fixed_string<detail::num_digits<base>(integer)> result;

    {
        std::intmax_t num = integer < 0 ? -integer : integer;

        for (auto&& it = result.rbegin(); it != result.rend(); ++it) {
            *it = digits[num % base];
            num /= base;
        }
    }

    if constexpr (integer < 0) {
        result[0] = '-';
    }

    return result;
}

}  // namespace detail

// Convert an integer to a fixed_string  at compile time (in arbitrary base).
template <std::intmax_t integer, int base = 10>
inline constexpr fixed_string ito_fs = detail::ito_fs<integer, base>();

namespace detail {

template <auto, typename>
struct expand;

template <auto value>
inline constexpr fixed_string super_impl =
    expand<value, std::make_index_sequence<value.size()>>::value;

template <>
inline constexpr fixed_string super_impl<'0'> = "\u2070";
template <>
inline constexpr fixed_string super_impl<'1'> = "\u00b9";
template <>
inline constexpr fixed_string super_impl<'2'> = "\u00b2";
template <>
inline constexpr fixed_string super_impl<'3'> = "\u00b3";
template <>
inline constexpr fixed_string super_impl<'4'> = "\u2074";
template <>
inline constexpr fixed_string super_impl<'5'> = "\u2075";
template <>
inline constexpr fixed_string super_impl<'6'> = "\u2076";
template <>
inline constexpr fixed_string super_impl<'7'> = "\u2077";
template <>
inline constexpr fixed_string super_impl<'8'> = "\u2078";
template <>
inline constexpr fixed_string super_impl<'9'> = "\u2079";
template <>
inline constexpr fixed_string super_impl<'-'> = "\u207b";

template <auto fs, std::size_t... Is>
struct expand<fs, std::index_sequence<Is...>> {
    static constexpr fixed_string value = {super_impl<fs[Is]>...};
};

}  // namespace detail

// convert a fixed string of numbers 0-9 & '-' into a superscript fixed_string
template <auto value>
inline constexpr fixed_string super = detail::super_impl<value>;

}  // namespace fs
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

#include <cstdint>  // std::intmax_t
#include <numeric>  // gcd
#include <ratio>
#include <type_traits>

namespace su {

// Convenience struct for inheriting: using type = ...
template <typename T = void>
struct Type {
    using type = T;
};

namespace detail {

struct scale_tag {};  // marks class as being of scale type

}  // namespace detail

template <std::intmax_t I = 1, std::intmax_t J = 1, std::intmax_t K = 0>
struct ScaleBase : private detail::scale_tag {
    using ratio = std::ratio<I, J>;

    static constexpr std::intmax_t num = ratio::num;
    static constexpr std::intmax_t den = ratio::den;
    static constexpr std::intmax_t exp = K;

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
struct scale<> : ScaleBase<> {};

template <std::intmax_t I>
struct scale<I> : ScaleBase<I> {};

template <std::intmax_t I, std::intmax_t J>
struct scale<I, J> : ScaleBase<I, J> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale<I, J, K> : ScaleBase<I, J, K> {};

namespace detail {

enum { multiply, done, divide };

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

// forward declaration for standard_form_impl
template <int C, std::intmax_t Exp, typename Ratio>
struct standard_match;

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl;

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl
    : standard_match<standard_direction<Ratio>(), Exp, Ratio> {};

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

}  // namespace detail

// convert a std::ratio to standard form, returns a struct with ::type = the
// standard form ratio and ::exp the base 10 exponent.
template <typename Ratio>
using standard_form = detail::standard_form_impl<0, Ratio>;

namespace detail {

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_impl : Type<scale<I, J, K>> {};

template <std::intmax_t I, std::intmax_t J>
struct scale_make_impl<I, J, 0> : Type<scale<I, J>> {};

template <std::intmax_t I>
struct scale_make_impl<I, 1, 0> : Type<scale<I>> {};

template <>
struct scale_make_impl<1, 1, 0> : Type<scale<>> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale_make_help {
    using standard = standard_form<std::ratio<I, J>>;
    using type = scale_make_impl<standard::type::num, standard::type::den,
                                 K + standard::exp>::type;
};

}  // namespace detail

// Returns the most minimal possible scale type in standard form
template <std::intmax_t I = 1, std::intmax_t J = 1, std::intmax_t K = 0>
using scale_make = detail::scale_make_help<I, J, K>::type;

namespace detail {

template <Scale A, Scale B>
struct scale_multiply {
    using product = std::ratio_multiply<typename A::ratio, typename B::ratio>;
    using type = scale_make<product::num, product::den, A::exp + B::exp>;
};

template <Scale A, Scale B>
struct scale_divide {
    using product = std::ratio_divide<typename A::ratio, typename B::ratio>;
    using type = scale_make<product::num, product::den, A::exp - B::exp>;
};

}  // namespace detail

template <Scale A, Scale B>
using scale_multiply_t = detail::scale_multiply<A, B>::type;

template <Scale A, Scale B>
using scale_divide_t = detail::scale_divide<A, B>::type;

namespace detail {

// Constexpr pow10 function
template <std::intmax_t exponent>
constexpr std::conditional_t<(exponent < 308), double, long double>
pow10_impl() {
    if constexpr (exponent == 0) {
        return 1.;
    } else if constexpr (exponent == 1) {
        return 10.;
    } else if constexpr (exponent % 2 == 0) {
        return pow10_impl<exponent / 2>() * pow10_impl<exponent / 2>();
    } else {
        return pow10_impl<exponent / 2>() * pow10_impl<exponent / 2>() * 10;
    }
}

template <std::intmax_t exponent>
constexpr auto pow10() {
    if constexpr (exponent > 0) {
        return pow10_impl<exponent>();
    } else {
        return 1. / pow10_impl<-exponent>();
    }
}

}  // namespace detail

// Could be generalised to arbitrary scale<...> !
template <Scale From, Scale To>
inline constexpr auto scale_convert(auto x) {
    using conversion = scale_divide_t<From, To>;

    constexpr std::intmax_t num = conversion::num;
    constexpr std::intmax_t den = conversion::den;
    constexpr std::intmax_t exp = conversion::exp;

    // Avoid floating point multiplication without -ffast-math
    if constexpr (exp == 0) {
        if constexpr (num == 1 && den == 1) {
            return x;
        } else if constexpr (num != 1 && den == 1) {
            return x * num;
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<double>(den);
        } else {
            return x * static_cast<double>(num) / den;
        }
    } else {
        // double or long double
        constexpr auto pow10 = detail::pow10<exp>();

        if constexpr (num == 1 && den == 1) {
            return x * pow10;
        } else if constexpr (num != 1 && den == 1) {
            return (x * num) * pow10;
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<double>(den) * pow10;
        } else {
            return x * static_cast<double>(num) / den * pow10;
        }
    }

    // if constexpr (den == 1) {
    //   // use of common_scale should ensure this den==1 for all implicit
    //    //  conversions
    //     if constexpr (num == 1) {
    //         if constexpr (exp == 0) {
    //             return x;  // null op
    //         } else constexpr {
    //             return 0;
    //         }
    //     } else {
    //     }
    // } else {
    //     if constexpr (num == 1) {
    //         if constexpr (exp == 0) {
    //         } else {
    //         }
    //     } else {
    //     }
    // }
}

namespace detail {

template <Scale S1, Scale S2>
struct common_scale_impl {
    static constexpr std::intmax_t gcd_num = std::gcd(S1::num, S2::num);

    static constexpr std::intmax_t gcd_den = std::gcd(S1::den, S2::den);

    // deliberate no use of make_scale to avoid standard form conversion
    using type = scale<gcd_num, (S1::den / gcd_den) * S2::den,
                       S1::exp <= S2::exp ? S1::exp : S2::exp>;
};

// Short-cut for same scales
template <Scale S>
struct common_scale_impl<S, S> : Type<S> {};

}  // namespace detail

// Returns the scale factor that is the greatest common multiple of S1 and S2's
// scale factors such that each scale is only scaled up in a conversion. This
// avoids integer division where possible. Return scale is not in standard form
// and therefore scale should used should not be used to make a quantity without
// first passing through scale_make
template <Scale S1, Scale S2>
using common_scale = detail::common_scale_impl<S1, S2>::type;

// Stringifys a scale<> into a minimal "(a/b x 10^c)" like form.
template <Scale S>
inline constexpr auto anotate() {
    using namespace fs;  // for string literal operator

    constexpr std::intmax_t num = S::num;
    constexpr std::intmax_t den = S::den;
    constexpr std::intmax_t exp = S::exp;

    if constexpr (exp == 0) {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return ""_fs;
            } else {
                return "("_fs + ito_fs<num> + ")"_fs;
            }
        } else {
            return "("_fs + ito_fs<num> + "/"_fs + ito_fs<den> + ")"_fs;
        }
    } else {
        if constexpr (den == 1) {
            if constexpr (num == 1) {
                return "(10"_fs + super<ito_fs<exp>> + ")"_fs;
            } else {
                return "("_fs + ito_fs<num> + "\u00D710"_fs +
                       super<ito_fs<exp>> + ")"_fs;
            }
        } else {
            return "("_fs + ito_fs<num> + "/"_fs + ito_fs<den> + "\u00D710"_fs +
                   super<ito_fs<exp>> + ")"_fs;
        }
    }
}

}  // namespace su
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

#include <cstddef>  // std::size_t
#include <cstdint>  // std::intmax_t
#include <ratio>
#include <type_traits>

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
requires ordered_v<A>&& ordered_v<B> using merge_sum_sorted_t =
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

// Compile time bottom-up merge sort a parameter pack of dimensions into a list
template <Dimension... Dims>
using sort_t = detail::sort_impl<list<>, list<Dims>...>::type;

}  // namespace su
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

#include <type_traits>

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
requires !std::is_same_v<typename U::scale_factor,
                         scale<>> struct downcast_unit_impl<U, U> : Type<U> {};

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

#include <ostream>
#include <type_traits>

#if __GNUC__ >= 10
#include <compare>
#endif

namespace su {

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

namespace detail {
struct quantity_tag {};
}  // namespace detail

template <typename T>
concept Quantity = std::is_base_of_v<detail::quantity_tag, T>;

template <Unit U, Arithmetic Rep = double>
class quantity;

// The libraries central type.
template <Unit U, Arithmetic Rep>
class quantity : private detail::quantity_tag {
   public:
    using value_type = Rep;
    using unit = U;

    quantity() = default;
    quantity(quantity const&) = default;
    quantity(quantity&&) = default;

    quantity& operator=(quantity const&) = default;
    quantity& operator=(quantity&&) = default;

    constexpr quantity(Arithmetic value) : m_value{value} {};

    // Conversions occur hear
    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity(
        quantity<U2, Rep2> other)
        : m_value{raw_convert<U2, U>(other.get())} {}

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator=(
        quantity<U2, Rep2> other) {
        m_value = quantity(other).get();
        return *this;
    }

    [[nodiscard]] static constexpr std::string_view symbol() noexcept {
        return unit::m_symbol.view();
    }

    [[nodiscard]] static constexpr std::string_view base_symbol() noexcept {
        return unit::m_base_symbol.view();
    }

    [[nodiscard]] constexpr Rep get() const noexcept { return m_value; }

    inline constexpr quantity operator-() const { return quantity(-get()); }

    constexpr quantity& operator++() {
        ++m_value;
        return *this;
    }

    constexpr quantity operator++(int) { return quantity(m_value++); }

    constexpr quantity& operator--() {
        --m_value;
        return *this;
    }

    constexpr quantity operator--(int) { return quantity(m_value--); }

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator+=(
        quantity<U2, Rep2> other) {
        m_value += quantity(other).get();
        return *this;
    }

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> constexpr quantity& operator-=(
        quantity<U2, Rep2> other) {
        m_value -= quantity(other).get();
        return *this;
    }

    constexpr quantity& operator*=(Arithmetic rhs) {
        m_value *= rhs;
        return *this;
    }

    constexpr quantity& operator/=(Arithmetic rhs) {
        m_value /= rhs;
        return *this;
    }

#if __GNUC__ >= 10

    template <Unit U2, Arithmetic Rep2>
    requires dimension_equal_v<U, U2> friend constexpr auto operator<=>(
        quantity lhs, quantity<U2, Rep2> rhs) {
        return get() <=> raw_convert<U2, U>(other.get());
    }

#endif

    // template <Unit U2, Arithmetic Rep2>
    // requires dimension_equal_v<U, U2> friend constexpr auto operator==(
    //     quantity lhs, quantity<U2, Rep2> rhs) r {
    //     return cq(lhs).count() == cq(rhs).count();
    // }

    friend std::ostream& operator<<(std::ostream& os, const quantity obj) {
        return os << obj.get();
    }

   private:
    Rep m_value;

    // hidden operators
};

/////////////////////////////////  operators //////////////////////////////////

template <Unit U1, Arithmetic R1, Unit U2, Arithmetic R2>
constexpr inline auto operator+(
    quantity<U1, R1> const lhs,
    quantity<U2, R2> const rhs) requires dimension_equal_v<U1, U2> {
    // Using a common quantity and constructors to do the conversion to avoid
    // division and provide symmetry to + operation. Also throws warnings in
    // case of narrowing conversions in constructors;
    using common_s =
        common_scale<typename U1::scale_factor, typename U2::scale_factor>;

    // int i = typename U1::scale_factor{};

    // Bypass unit_make_t to prevent conversion to standard form of scale.
    using common_q =
        quantity<typename detail::unit_make_impl<true, common_s,
                                                 typename U1::dimensions>::type,
                 std::common_type_t<R1, R2>>;

    using rep = decltype(common_q(lhs).get() + common_q(rhs).get());

    // Now use unit_make_t to simplify scale.
    using quantity_t =
        quantity<downcast_unit<unit_make_t<common_s, typename U1::dimensions>>,
                 rep>;

    return quantity_t{common_q(lhs).get() + common_q(rhs).get()};
}

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator-(
//     quantity<Ul, Tl> const lhs,
//     quantity<Ur, Tr> const rhs) requires dimension_equal_v<Ul, Ur> {
//     //
//     using quantity_t =
//         quantity<Ul, decltype(lhs.get() - raw_convert<Ur, Ul>(rhs.get()))>;

//     return quantity_t{lhs.get() - raw_convert<Ur, Ul>(rhs.get())};
// }

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator*(quantity<Ul, Tl> const lhs,
//                                 quantity<Ur, Tr> const rhs) {
//     //
//     using unit_t = unit_make_from_sorted_t<
//         scale_multiply_t<typename Ul::scale_factor, typename
//         Ur::scale_factor>, merge_sum_sorted_t<typename Ul::dimensions,
//         typename Ur::dimensions>>;

//     using quanity_t =
//         quantity<downcast_unit<unit_t>, decltype(lhs.get() * rhs.get())>;

//     return quanity_t{lhs.get() * rhs.get()};
// }

// template <Unit Ul, Arithmetic Tl, Unit Ur, Arithmetic Tr>
// constexpr inline auto operator/(quantity<Ul, Tl> const lhs,
//                                 quantity<Ur, Tr> const rhs) {
//     //
//     using dimensions = merge_sum_sorted_t<
//         typename Ul::dimensions,
//         dimension_multiply_t<typename Ur::dimensions, std::ratio<-1>>>;

//     using unit_t = unit_make_from_sorted_t<
//         scale_divide_t<typename Ul::scale_factor, typename Ur::scale_factor>,
//         dimensions>;

//     using quanity_t =
//         quantity<downcast_unit<unit_t>, decltype(lhs.get() / rhs.get())>;

//     return quanity_t{lhs.get() / rhs.get()};
// }

// constexpr inline auto operator*(Quantity lhs, Arithmetic rhs) {
//     return lhs * quantity<nameless<scale<>>, decltype(rhs)>{rhs};
// }

// constexpr inline auto operator*(Arithmetic lhs, Quantity rhs) {
//     return quantity<nameless<scale<>>, decltype(lhs)>{lhs} * rhs;
// }

// constexpr inline auto operator/(Quantity lhs, Arithmetic rhs) {
//     return lhs / quantity<nameless<scale<>>, decltype(rhs)>{rhs};
// }

// constexpr inline auto operator/(Arithmetic lhs, Quantity rhs) {
//     return quantity<nameless<scale<>>, decltype(lhs)>{lhs} / rhs;
// }

}  // namespace su
#include <iostream>

// namespace su::nope {

// #pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wnon-template-friend"

// template <typename BaseType>
// struct downcast_base {
//     using base_type = BaseType;

//     friend auto downcast_guide(downcast_base);
// };

// #pragma GCC diagnostic pop

// template <Scale S, typename... Ts>
// struct UNIT : downcast_base<UNIT<S, Ts...>> {};

// template <typename T>
// concept Downcastable = requires {
//     typename T::base_type;
// }
// &&std::is_base_of_v<downcast_base<typename T::base_type>, T>;

// template <typename Target, Downcastable T>
// struct downcast_helper : T {
//     friend auto downcast_guide(typename downcast_helper::downcast_base) {
//         return Target();
//     }
// };

// template <typename Child, typename... Es>
// struct derived_dimension : downcast_helper<Child, UNIT<Es...>> {};

// template <typename T>
// concept has_downcast = requires {
//     downcast_guide(std::declval<downcast_base<T>>());
// };

// template <typename T>
// constexpr auto downcast_target_impl() {
//     if constexpr (has_downcast<T>) {
//         return decltype(downcast_guide(std::declval<downcast_base<T>>()))();
//     } else {
//         return T{};
//     }
// }

// template <Downcastable T>
// using downcast_target = decltype(downcast_target_impl<T>());

// ////////////////////////////////////////////////////////

// template <template <typename> typename CRTP, fs::fixed_string Sym, Arithmetic
// T,
//           Scale S, Dimension... Dims>

// struct named_unit : unit_make<T, S, Dims...> {
//     static constexpr auto self = named_unit<CRTP, Sym, T, S, Dims...>{};

//     using unit_make<T, S, Dims...>::unit;
//     using unit_make<T, S, Dims...>::operator=;

//     [[nodiscard]] inline static constexpr std::string_view symbol() noexcept
//     {
//         return m_symbol.view();
//     }

//     static constexpr fs::fixed_string m_symbol = Sym;
// };

// struct timer : derived_dimension<timer, scale<>, si::time<>> {};

// struct velocity : derived_dimension<velocity, scale<>, length<>> {};

// // struct minute: named_unit<minute, "min", su::scale<60>, si::second<>>;

// // quant<minute, double> elapsed_time;

// // quant<hour> final = elapsed_time

// template <typename T>
// struct conor : named_unit<conor, "conors", double, scale<>, length<>> {
//     using named_unit<conor, "conors", double, scale<>, length<>>::named_unit;
//     using named_unit<conor, "conors", double, scale<>, length<>>::operator=;
// };

// }  // namespace su::nope

namespace si {

// TODO : check all pass by value no const
// TODO : static constexpr in functions

// clang-format off
template <std::intmax_t... Is> struct              length : su::DimensionBase<  "m", Is...> {};
template <std::intmax_t... Is> struct                time : su::DimensionBase<  "s", Is...> {};
template <std::intmax_t... Is> struct                mass : su::DimensionBase< "kg", Is...> {};
template <std::intmax_t... Is> struct             current : su::DimensionBase<  "A", Is...> {};
template <std::intmax_t... Is> struct          temprature : su::DimensionBase<  "K", Is...> {};
template <std::intmax_t... Is> struct              amount : su::DimensionBase<"mol", Is...> {};
template <std::intmax_t... Is> struct  luminous_intensity : su::DimensionBase< "cd", Is...> {};

struct    scalar : su::named_unit<   scalar,      "scalar",        su::scale<>> {};

struct    metres : su::unit<   metres, su::scale<>,             length<>> {};
struct   seconds : su::unit<  seconds, su::scale<>,               time<>> {};
struct kilograms : su::unit<kilograms, su::scale<>,               mass<>> {};
struct   amperes : su::unit<  amperes, su::scale<>,            current<>> {};
struct    kelvin : su::unit<   kelvin, su::scale<>,         temprature<>> {};
struct     moles : su::unit<    moles, su::scale<>,             amount<>> {};
struct  candelas : su::unit< candelas, su::scale<>, luminous_intensity<>> {};


struct     hertz : su::named_unit<hertz, "Hz", su::scale<>, time<-1>> {};

struct kilometres : su::named_unit<kilometres, "km",  su::scale<1,1,3>,  length<>> {};

struct meters_per_second : su::unit<meters_per_second, su::scale<>, length<>, time<-1>  > {};

// clang-format on

}  // namespace si

#include <numeric>

using su::quantity;

struct a : su::named_unit<a, "a", su::scale<30, 7, -1>, si::length<>> {};
struct b : su::named_unit<b, "b", su::scale<5, 2>, si::length<>> {};

inline constexpr quantity<si::seconds, double> bear_oclock{12.};

// quantity operators

int main() {
    quantity<a, double> m{1.};

    quantity<b, double> km{1.};

    quantity<si::metres> sum = m + km;

    // int i = km + m;

    std::cout << m + km << ' ' << (m + km).base_symbol() << std::endl;
    std::cout << km << ' ' << (km).symbol() << std::endl;
    std::cout << sum << ' ' << (sum).symbol() << std::endl;

    //
    int num1 = 51;
    int den1 = 10;
    int Exp1 = 1;

    int num2 = 5;
    int den2 = 1;
    int Exp2 = 0;

    std::intmax_t gcd_num = std::gcd(num1, num2);
    std::intmax_t gcd_den = std::gcd(den1, den2);

    std::cout << gcd_num << '/' << (den1 / gcd_den) * den2 << '^'
              << std::min(Exp1, Exp2) << std::endl;

    return 0;
}
