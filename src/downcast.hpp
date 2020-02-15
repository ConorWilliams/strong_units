// The MIT License (MIT)
//
// Copyright (c) 2018 Mateusz Pusz
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
//
//
// This file is taken from: https://github.com/mpusz/units
//
// Full credit to Mateusz Pusz for this fantastic idiom!
//
// Implements the (very clever) downcast template-idiom enabling (where =>
// denotes inheritance):
//
// struct parent => unit<i, j, k> => downcast_base<unit<i, j, k>>
//
// downcast(unit<i, j, k>) -> parent
//
// This is achieved by parent inheriting unit<...> through downcast_child which
// specialises a friend function defined in downcast base. downcast_child has
// full knowledge of parent through the CRTP idiom and can specialise the friend
// function in downcast base to return it. Full inheritance:
//
// parent => make_unit_helper<parent ...> => downcast_child<parent, ...> =>
// unit<...> => downcast_base<unit<...>>

#pragma once

#include <type_traits>

namespace su {

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnon-template-friend"

template <typename BaseType>
struct downcast_base {
    using downcast_base_type = BaseType;
    friend auto downcast_guide(downcast_base);
};

#pragma GCC diagnostic pop

template <typename T>
concept Downcastable = requires {
    typename T::downcast_base_type;
}
&&std::is_base_of_v<downcast_base<typename T::downcast_base_type>, T>;

template <typename Target, Downcastable T>
struct downcast_child : T {
    friend auto downcast_guide(typename downcast_child::downcast_base) {
        return Target();
    }
};

namespace detail {

template <typename T>
concept has_downcast = requires {
    downcast_guide(std::declval<downcast_base<T>>());
};

template <typename T>
constexpr auto downcast_impl() {
    if constexpr (has_downcast<T>) {
        return decltype(downcast_guide(std::declval<downcast_base<T>>()))();
    } else {
        return T();
    }
}

}  // namespace detail

template <Downcastable T>
using downcast = decltype(detail::downcast_impl<T>());

}  // namespace su
