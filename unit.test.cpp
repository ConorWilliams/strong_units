#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define DOCTEST_CONFIG_SUPER_FAST_ASSERTS
#include "doctest.h"

#include "fixed_string.hpp"

using namespace fs;

TEST_CASE("fixed_string class") {
    constexpr fixed_string empty;

    CHECK(empty.empty());

    CHECK(empty.size() == 0);
    CHECK(decltype(empty)::size() == 0);

    CHECK(empty.view() == "");

    constexpr fixed_string one = "x";
    constexpr fixed_string two = "xx";
    constexpr fixed_string three = "xxx";

    CHECK(!one.empty());
    CHECK(one.size() == 1);

    CHECK(three == one + two);
    CHECK(three == one + one + one);

    CHECK(three == fixed_string{one, one, one});
    CHECK(three == fixed_string{two, one});

    constexpr fixed_string five = "axxxe";

    CHECK(five.view(1, 4) == "xxx");
    CHECK(five.view(1, 4) == three.view());

    int count = 0;
    for (auto&& elem : "0123456789"_fs) {
        CHECK(elem == '0' + count++);
    }

    fixed_string change = "xxx";

    CHECK(change == three);

    change[0] = 'a';
    change[1] = 'b';
    change[2] = 'c';

    CHECK(change == fixed_string{"abc"});
}

template <fixed_string str>
struct custom_class {
    static constexpr fixed_string data = str;
};

TEST_CASE("fixed_string template") {
    using cool = custom_class<"cool!">;

    CHECK(cool::data == fixed_string{"cool!"});
}

TEST_CASE("fixed_string compare") {
    constexpr fixed_string a = "a";
    constexpr fixed_string b = "b";
    constexpr fixed_string two = "xx";

    constexpr int tmp = compare(a, b);

    CHECK(compare(a, a) == 0);
    CHECK(compare(a, b) == -1);
    CHECK(compare(b, a) == 1);

    CHECK(compare(b, fixed_string{"b"}) == 0);

    CHECK(compare(a, two) == -1);
    CHECK(compare(b, two) == -1);

    CHECK(compare(two, a) == 1);
    CHECK(compare(two, b) == 1);

    CHECK(compare(fixed_string{"xa"}, fixed_string{"xb"}) == -1);
    CHECK(compare(fixed_string{"xb"}, fixed_string{"xa"}) == 1);
}

TEST_CASE("fixed_string ito_fs") {
    constexpr fixed_string zero = "0";
    constexpr fixed_string plus = "12345";
    constexpr fixed_string minus = "-12345";

    CHECK(ito_fs<0> == zero);
    CHECK(ito_fs<12345> == plus);
    CHECK(ito_fs<-12345> == minus);
}
