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

template <std::intmax_t exponent, typename T>
requires(exponent > 0) constexpr T pow10() {
    if constexpr (exponent == 0) {
        return T(1);
    } else if constexpr (exponent == 1) {
        return T(10);
    } else if constexpr (exponent % 2 == 0) {
        return pow10<exponent / 2, T>() * pow10<exponent / 2, T>();
    } else {
        return pow10<exponent / 2, T>() * pow10<exponent / 2, T>() * T(10);
    }
}

}  // namespace detail

template <Scale From, Scale To, typename T>
inline constexpr T scale_convert(T x) {
    //
    using ratio = std::ratio_divide<typename From::ratio, typename To::ratio>;

    constexpr std::intmax_t num = ratio::num;
    constexpr std::intmax_t den = ratio::den;
    constexpr std::intmax_t exp = From::exp - To::exp;

    // Avoid floating point multiplication without -ffast-math
    if constexpr (exp == 0) {
        if constexpr (num == 1 && den == 1) {
            return x;
        } else if constexpr (num != 1 && den == 1) {
            return x * static_cast<T>(num);
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<T>(den);
        } else if constexpr (num != 1 && den != 1) {
            return x * static_cast<T>(num) / static_cast<T>(den);
        }
    } else if constexpr (exp > 0) {
        constexpr T pow10 = detail::pow10<exp, T>();

        if constexpr (num == 1 && den == 1) {
            return x * pow10;
        } else if constexpr (num != 1 && den == 1) {
            return x * static_cast<T>(num) * pow10;
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<T>(den) * pow10;
        } else if constexpr (num != 1 && den != 1) {
            return x * static_cast<T>(num) / static_cast<T>(den) * pow10;
        }
    } else if constexpr (exp < 0) {
        constexpr T pow10 = detail::pow10<-exp, T>();

        if constexpr (num == 1 && den == 1) {
            return x / pow10;
        } else if constexpr (num != 1 && den == 1) {
            return x * static_cast<T>(num) / pow10;
        } else if constexpr (num == 1 && den != 1) {
            return x / static_cast<T>(den) / pow10;
        } else if constexpr (num != 1 && den != 1) {
            return x * static_cast<T>(num) / static_cast<T>(den) / pow10;
        }
    }
}

}  // namespace su