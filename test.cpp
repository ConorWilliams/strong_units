#include <cstdint>  // std::intmax_t
#include <iostream>
#include <ratio>
#include <type_traits>

// Convenience struct for inheriting: using type = ...
template <typename T = void>
struct Type {
    using type = T;
};

namespace detail {

// if     Ratio <= -10       =>  1
// if     -10 < Ratio <= -1  =>  0
// if     -1 < Rato < 0      =>  -1
// if     Ratio == 0         =>  0
// if     Ratio < 1          => -1
// if     1 <= Ratio < 10    =>  0
// if     Ratio >= 10        =>  1
template <typename Ratio>
constexpr int standard_direction() {
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-10>>) {
        return 1;
    }
    if constexpr (std::ratio_less_equal_v<Ratio, std::ratio<-1>>) {
        return 0;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<0>>) {
        return -1;
    }
    if constexpr (std::ratio_equal_v<Ratio, std::ratio<0>>) {
        return 0;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<1>>) {
        return -1;
    }
    if constexpr (std::ratio_less_v<Ratio, std::ratio<10>>) {
        return 0;
    }
    if constexpr (std::ratio_greater_equal_v<Ratio, std::ratio<10>>) {
        return 1;
    }
}

// forward declaration
template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl;

// end condition
template <int C, std::intmax_t Exp, typename Ratio>
struct standard_match {
    static constexpr std::intmax_t exp = Exp;
    using type = Ratio;
};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<1, Exp, Ratio>
    : standard_form_impl<Exp + 1, std::ratio_divide<Ratio, std::ratio<10>>> {};

template <std::intmax_t Exp, typename Ratio>
struct standard_match<-1, Exp, Ratio>
    : standard_form_impl<Exp - 1, std::ratio_multiply<Ratio, std::ratio<10>>> {
};

template <std::intmax_t Exp, typename Ratio>
struct standard_form_impl
    : standard_match<standard_direction<Ratio>(), Exp, Ratio> {};

}  // namespace detail

template <typename Ratio>
using standard_form = detail::standard_form_impl<0, Ratio>;

int main() {
    using simple = standard_form<std::ratio<-100000, 11>>;

    std::cout << simple::type::num << " / " << simple::type::den << " ^ "
              << simple::exp << std::endl;
    return 0;
}