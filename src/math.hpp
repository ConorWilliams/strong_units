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
#include <cassert>
#include <cstdint>  // std::intmax_t
#include <limits>
#include <numeric>

namespace su {

namespace detail {

// Computes (a * b) mod m relies on unsigned integer arithmetic
constexpr std::uint64_t mulmod(std::uint64_t a, std::uint64_t b,
                               std::uint64_t m) {
    std::uint64_t res = 0;

    if (b >= m) {
        if (m > UINT64_MAX / 2u) {
            b -= m;
        } else {
            b %= m;
        }
    }

    while (a != 0) {
        if (a & 1) {
            if (b >= m - res) {
                res -= m;
            }
            res += b;
        }
        a >>= 1;

        std::uint64_t temp_b = b;
        if (b >= m - b) {
            temp_b -= m;
        }
        b += temp_b;
    }

    return res;
}

// Calculates (a ^ e) mod m
constexpr std::uint64_t modpow(std::uint64_t a, std::uint64_t e,
                               std::uint64_t m) {
    a %= m;
    std::uint64_t result = 1;

    while (e > 0) {
        if (e & 1) {
            result = mulmod(result, a, m);
        }
        a = mulmod(a, a, m);
        e >>= 1;
    }
    return result;
}

// gcd(a * 10 ^ e, b)
constexpr std::intmax_t gcdpow(std::intmax_t a, std::intmax_t e,
                               std::intmax_t b) noexcept {
    assert(a > 0);
    assert(e >= 0);
    assert(b > 0);

    // gcd(i, j) = gcd(j, i mod j) for j != 0 Euclid;
    //
    // gcd(a 10^e, b) = gcd(b, a 10^e mod b)
    //
    // (a 10^e) mod b -> [ (a mod b) (10^e mod b) ] mod b

    return std::gcd(b, mulmod(a % b, modpow(10, e, b), b));
}

constexpr void cwap(std::intmax_t &lhs, std::intmax_t &rhs) {
    std::intmax_t tmp = lhs;
    lhs = rhs;
    rhs = tmp;
}

}  // namespace detail

// Computes the rational gcd of n1/d1 x 10^e1 and n2/d2 x 10^e2
constexpr auto gcd_frac(std::intmax_t n1, std::intmax_t d1, std::intmax_t e1,
                        std::intmax_t n2, std::intmax_t d2, std::intmax_t e2) {
    if (e2 > e1) {
        detail::cwap(n1, n2);
        detail::cwap(d1, d2);
        detail::cwap(e1, e2);
    }

    std::intmax_t exp = e2;  // minimum

    std::intmax_t num = detail::gcdpow(n1, e1 - e2, n2);

    if (d1 < d2) {
        detail::cwap(d1, d2);
    }

    std::intmax_t tmp = detail::gcdpow(d1, e1 - e2, d2);

    assert(std::numeric_limits<std::intmax_t>::max() / d1 > d2);
    assert((d1 * d2) % tmp == 0);

    std::intmax_t den = (d1 * d2) / tmp;

    return std::array{num, den, exp};
}

}  // namespace su