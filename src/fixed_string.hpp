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

    auto operator<=>(const fixed_string&) const = default;

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
std::ostream& operator<<(std::ostream& os, fixed_string<N> const& str) {
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
requires(base > 1) constexpr std::size_t num_digits(std::intmax_t x) {
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
template <fixed_string value>
inline constexpr fixed_string super = detail::super_impl<value>;

}  // namespace fs
