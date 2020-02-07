#include <iostream>

#include "unit.hpp"

struct scal {
    int a = 1, b = 1, c = 0, d = 1;
};

int main() {
    constexpr auto m = meter{1, 2};
    constexpr auto s = second{1};

    constexpr int t = str_compare(s.symbol, m.symbol);

    list<1, 4, 6> l;

    Unit<int, scal{1, 2, 3, 4}, meter{1, 2}, second{1, 3}> k;

    Unit<int, second{3}, meter{1}> b{2};

    Unit<double, scal{}> stick{45};

    int i = b;

    b + b;

    std::cout << (b + b).value << std::endl;
    return 0;
}