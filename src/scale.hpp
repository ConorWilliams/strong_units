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

#include <cstdint>  // std::intmax_t
#include <numeric>  // gcd
#include <ratio>
#include <type_traits>

#include "fixed_string.hpp"

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