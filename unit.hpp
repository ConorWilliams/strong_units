#pragma once

#include <algorithm>
#include <cstdint>
#include <numeric>
#include <type_traits>

#include "meta.hpp"

class str_const {
   private:
    const char* const p_;
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

constexpr std::intmax_t sign(std::intmax_t x) { return x < 0 ? -1 : 1; }
constexpr std::intmax_t absol(std::intmax_t x) { return sign(x) * x; }

template <typename T>
concept Dimension = requires(T x) {
    x.num;
    x.den;
    x.symbol;
};

template <auto x>
concept Dimension_v = Dimension<decltype(x)>;

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

template <Dimension... Dims>
constexpr bool non_zero(Dims... dims) {
    return (... && (dims.num != 0)) && (... && (dims.den != 0));
}

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

template <typename T>
concept Unit_t = requires {
    typename T::dimension;
};

template <Arithmetic T, auto G, Dimension_v... Dims>
    requires ordered(Dims...) && non_zero(Dims...) struct Unit {
    using dimension = list<Dims...>;

    T value;

    // TODO: https://en.cppreference.com/w/cpp/language/operator_arithmetic

    template <Unit_t U>
    inline constexpr auto operator+(
        U other) requires std::is_same_v<dimension, typename U::dimension> {
        return Unit<decltype(value + other.value), 3, Dims...>{value +
                                                               other.value};
    }
};

/////////// unit / dimension examples ////////////////

struct meter {
    static constexpr str_const symbol = "m";

    const std::intmax_t num;
    const std::intmax_t den;

    constexpr meter(std::intmax_t Num, std::intmax_t Denom = 1)
        : num{sign(Num) * sign(Denom) * absol(Num) / std::gcd(Num, Denom)},
          den{absol(Denom) / std::gcd(Num, Denom)} {}
};

struct second {
    static constexpr str_const symbol = "s";

    const std::intmax_t num;
    const std::intmax_t den;

    constexpr second(std::intmax_t Num, std::intmax_t Denom = 1)
        : num{sign(Num) * sign(Denom) * absol(Num) / std::gcd(Num, Denom)},
          den{absol(Denom) / std::gcd(Num, Denom)} {}
};
