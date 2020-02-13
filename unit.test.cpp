#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define DOCTEST_CONFIG_SUPER_FAST_ASSERTS
#include "doctest.h"

#include "unit.hpp"

using namespace unit;

TEST_CASE("standard form") {
    // edge cases -10, 1, 0, 1, 10

    using zero = standard_form<std::ratio<0>>;

    CHECK(std::ratio_equal_v<typename zero::type, std::ratio<0>>);
    CHECK(zero::exp == 0);

    using one = standard_form<std::ratio<1>>;
    using m_one = standard_form<std::ratio<-1>>;

    CHECK(std::ratio_equal_v<typename one::type, std::ratio<1>>);
    CHECK(one::exp == 0);

    CHECK(std::ratio_equal_v<typename m_one::type, std::ratio<-1>>);
    CHECK(m_one::exp == 0);

    using mid = standard_form<std::ratio<15, 7>>;
    using m_mid = standard_form<std::ratio<-15, 7>>;

    CHECK(std::ratio_equal_v<typename mid::type, std::ratio<15, 7>>);
    CHECK(mid::exp == 0);

    CHECK(std::ratio_equal_v<typename m_mid::type, std::ratio<-15, 7>>);
    CHECK(m_mid::exp == 0);

    using ten = standard_form<std::ratio<10>>;
    using m_ten = standard_form<std::ratio<-10>>;

    CHECK(std::ratio_equal_v<typename ten::type, std::ratio<1>>);
    CHECK(ten::exp == 1);

    CHECK(std::ratio_equal_v<typename m_ten::type, std::ratio<-1>>);
    CHECK(m_ten::exp == 1);

    using large = standard_form<std::ratio<13000, 7>>;
    using m_large = standard_form<std::ratio<-13000, 7>>;

    CHECK(std::ratio_equal_v<typename large::type, std::ratio<13, 7>>);
    CHECK(large::exp == 3);

    CHECK(std::ratio_equal_v<typename m_large::type, std::ratio<-13, 7>>);
    CHECK(m_large::exp == 3);
}

TEST_CASE("scale_make") {
    CHECK(std::is_same_v<scale_make<0, 1, 3>, scale<-1, 10, 3>>);
    CHECK(std::is_same_v<scale_make<0, 2, 1>, scale<0, 2>>);
    CHECK(std::is_same_v<scale_make<4, 1, 1>, scale<4>>);
    CHECK(std::is_same_v<scale_make<0, 1, 1>, scale<>>);
}

TEST_CASE("scale / & *") {
    using one = scale_make<0, 1, 1>;
    using five = scale_make<0, 5, 1>;
    using twenty_five = scale_make<0, 25, 1>;

    CHECK(std::is_same_v<scale_multiply_t<one, one>, one>);
    CHECK(std::is_same_v<scale_multiply_t<one, five>, five>);
    CHECK(std::is_same_v<scale_multiply_t<five, one>, five>);
    CHECK(std::is_same_v<scale_multiply_t<five, five>, twenty_five>);

    CHECK(std::is_same_v<scale_divide_t<one, one>, one>);
    CHECK(std::is_same_v<scale_divide_t<one, five>, scale_make<0, 1, 5>>);
    CHECK(std::is_same_v<scale_divide_t<five, one>, five>);
    CHECK(std::is_same_v<scale_divide_t<five, five>, one>);
    CHECK(std::is_same_v<scale_divide_t<twenty_five, five>, five>);
}

TEST_CASE("pow10") {
    CHECK(detail::pow10<0>() == doctest::Approx(1));

    CHECK(detail::pow10<1>() == doctest::Approx(1e1));
    CHECK(detail::pow10<10>() == doctest::Approx(1e10));
    CHECK(detail::pow10<308>() == doctest::Approx(1e308));

    CHECK(detail::pow10<-1>() == doctest::Approx(1e-1));
    CHECK(detail::pow10<-10>() == doctest::Approx(1e-10));
    CHECK(detail::pow10<-308>() == doctest::Approx(1e-308));
}