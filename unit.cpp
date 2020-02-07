#include <iostream>

#include "unit2.hpp"

template <typename T>
using newton = unit<T, scale<>, meter<>, second<-2>, kilogram<>>;

int main() {
    unit<int, scale<>, ampere<>, meter<2, 4>, second<>> b{2};
    unit<int, scale<>, ampere<>, meter<1, 2>, second<>> k{3};

    // int i = newton<int>{};

    std::cout << b + k << std::endl;

    return 0;
}