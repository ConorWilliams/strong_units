#pragma once

// Copyright (c) 2020 Conor Williams

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include "unit.hpp"

// This file is part of the sUnit library and contains a full definition of
// the SI unit system in sUnit types.

#ifndef UNIT_DEFAULT_TYPE
#define UNIT_DEFAULT_TYPE double
#endif

#define UNIT_VALUE_ONE \
    UNIT_DEFAULT_TYPE { 1 }

// SI 7 base dimensions, '_d' suffix marks dimension.

template <std::intmax_t... Is>
struct meter_d : unit::DimensionBase<"m", Is...> {};

template <std::intmax_t... Is>
struct second_d : unit::DimensionBase<"s", Is...> {};

template <std::intmax_t... Is>
struct kilogram_d : unit::DimensionBase<"kg", Is...> {};

template <std::intmax_t... Is>
struct ampere_d : unit::DimensionBase<"A", Is...> {};

template <std::intmax_t... Is>
struct kelvin_d : unit::DimensionBase<"K", Is...> {};

template <std::intmax_t... Is>
struct mole_d : unit::DimensionBase<"mol", Is...> {};

template <std::intmax_t... Is>
struct candela_d : unit::DimensionBase<"cd", Is...> {};

// Additional dimension for radians (Non SI compliant)

// SI base unit types, unit types follow the _t suffix convention

template <typename T>
using meter_t = unit::unit<T, unit::scale<>, meter_d<>>;

template <typename T>
using second_t = unit::unit<T, unit::scale<>, second_d<>>;

template <typename T>
using kilogram_t = unit::unit<T, unit::scale<>, kilogram_d<>>;

template <typename T>
using ampere_t = unit::unit<T, unit::scale<>, ampere_d<>>;

template <typename T>
using kelvin_t = unit::unit<T, unit::scale<>, kelvin_d<>>;

template <typename T>
using mole_t = unit::unit<T, unit::scale<>, mole_d<>>;

template <typename T>
using candela_t = unit::unit<T, unit::scale<>, candela_d<>>;

// Concepts for SI base dimensions

template <typename T>
concept Length = unit::Unit<T>&& unit::dimension_equal_v<T, meter_t<int>>;

template <typename T>
concept Time = unit::Unit<T>&& unit::dimension_equal_v<T, second_t<int>>;

template <typename T>
concept Mass = unit::Unit<T>&& unit::dimension_equal_v<T, kilogram_t<int>>;

template <typename T>
concept Current = unit::Unit<T>&& unit::dimension_equal_v<T, ampere_t<int>>;

template <typename T>
concept Temprature = unit::Unit<T>&& unit::dimension_equal_v<T, kelvin_t<int>>;

template <typename T>
concept Amount = unit::Unit<T>&& unit::dimension_equal_v<T, mole_t<int>>;

template <typename T>
concept Intensity = unit::Unit<T>&& unit::dimension_equal_v<T, kilogram_t<int>>;

// SI base unit literal instances
//
// clang-format off
inline constexpr    meter_t<UNIT_DEFAULT_TYPE>    meter {UNIT_VALUE_ONE};
inline constexpr   second_t<UNIT_DEFAULT_TYPE>   second {UNIT_VALUE_ONE};
inline constexpr kilogram_t<UNIT_DEFAULT_TYPE> kilogram {UNIT_VALUE_ONE};
inline constexpr   ampere_t<UNIT_DEFAULT_TYPE>   ampere {UNIT_VALUE_ONE};
inline constexpr   kelvin_t<UNIT_DEFAULT_TYPE>   kelvin {UNIT_VALUE_ONE};
inline constexpr     mole_t<UNIT_DEFAULT_TYPE>     mole {UNIT_VALUE_ONE};
inline constexpr  candela_t<UNIT_DEFAULT_TYPE>  candela {UNIT_VALUE_ONE};

namespace prefix {

using yotta = unit::scale<24>;
using zetta = unit::scale<21>;
using   exa = unit::scale<18>;
using  peta = unit::scale<15>;
using  tera = unit::scale<12>;
using  giga = unit::scale<9>;
using  mega = unit::scale<6>;
using  kilo = unit::scale<3>;
using hecto = unit::scale<2>;
using  deka = unit::scale<1>;
using  deci = unit::scale<-1>;
using centi = unit::scale<-2>;
using milli = unit::scale<-3>;
using micro = unit::scale<-6>;
using  nano = unit::scale<-9>;
using  pico = unit::scale<-12>;
using femto = unit::scale<-15>;
using  atto = unit::scale<-18>;
using zepto = unit::scale<-21>;
using yocto = unit::scale<-24>;

using  kibi = unit::scale_make<1, 1024, 1>;
using  mebi = unit::scale_multiply_t<kibi, kibi>;
using  gibi = unit::scale_multiply_t<mebi, kibi>;
using  tebi = unit::scale_multiply_t<gibi, kibi>;
using  pebi = unit::scale_multiply_t<tebi, kibi>;
using  exbi = unit::scale_multiply_t<pebi, kibi>;
using  zebi = unit::scale_multiply_t<exbi, kibi>;
using  yobi = unit::scale_multiply_t<zebi, kibi>;

// clang-format on

}  // namespace prefix
