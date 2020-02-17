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

#include "unit.hpp"  // clang-format off

// This file is part of the dUnit library and contains a full definition of
// the SI unit system in strong_unit types.

namespace si {

// SI 7 base dimensions.

template <std::intmax_t... Is> struct              length : su::dimension<  "m", Is... >{};
template <std::intmax_t... Is> struct                time : su::dimension<  "s", Is... >{};
template <std::intmax_t... Is> struct                mass : su::dimension< "kg", Is... >{};
template <std::intmax_t... Is> struct             current : su::dimension<  "A", Is... >{};
template <std::intmax_t... Is> struct          temprature : su::dimension<  "K", Is... >{};
template <std::intmax_t... Is> struct           substance : su::dimension<"mol", Is... >{};
template <std::intmax_t... Is> struct  luminous_intensity : su::dimension< "cd", Is... >{};

// Non SI extensions

template <std::intmax_t... Is> struct         information : su::dimension<  "b", Is... >{};
template <std::intmax_t... Is> struct               angle : su::dimension<"rad", Is... >{};

// SI base units 

struct    metre : su::simple_unit<    metre, su::scale<>,             length<> >{};
struct   second : su::simple_unit<   second, su::scale<>,               time<> >{};
struct kilogram : su::simple_unit< kilogram, su::scale<>,               mass<> >{};
struct   ampere : su::simple_unit<   ampere, su::scale<>,            current<> >{};
struct   kelvin : su::simple_unit<   kelvin, su::scale<>,         temprature<> >{};
struct     mole : su::simple_unit<     mole, su::scale<>,          substance<> >{};
struct  candela : su::simple_unit<  candela, su::scale<>, luminous_intensity<> >{};

// Non SI base units

struct scalar : su::simple_unit< scalar, su::scale<>                >{};
struct radian : su::simple_unit< radian, su::scale<>,       angle<> >{};
struct    bit : su::simple_unit<    bit, su::scale<>, information<> >{};


// Named SI units

struct steradian : su::symbol_unit< steradian,  "sr", su::scale<>,  angle<2>                                        >{};
struct     hertz : su::symbol_unit<     hertz,  "Hz", su::scale<>,  time<-1>                                        >{};
struct   sievert : su::symbol_unit<   sievert,  "Sv", su::scale<>, length<2>,    time<-2>                           >{};
struct     katal : su::symbol_unit<     katal, "kat", su::scale<>,  time<-1>, substance<>                           >{};
struct   coulomb : su::symbol_unit<   coulomb,   "C", su::scale<>,    time<>,   current<>                           >{};
struct    newton : su::symbol_unit<    newton,   "N", su::scale<>,    mass<>,    length<>,    time<-2>              >{};
struct    pascal : su::symbol_unit<    pascal,  "Pa", su::scale<>,    mass<>,  length<-1>,    time<-2>              >{};
struct     joule : su::symbol_unit<     joule,   "J", su::scale<>,    mass<>,   length<2>,    time<-2>              >{};
struct      watt : su::symbol_unit<      watt,   "W", su::scale<>,    mass<>,   length<2>,    time<-3>              >{};
struct     tesla : su::symbol_unit<     tesla,   "T", su::scale<>,    mass<>,    time<-2>, current<-1>              >{};
struct      volt : su::symbol_unit<      volt,   "V", su::scale<>,    mass<>,   length<2>,    time<-3>, current<-1> >{};
struct     farad : su::symbol_unit<     farad,   "F", su::scale<>,  mass<-1>,  length<-2>,     time<4>,  current<2> >{};
struct       ohm : su::symbol_unit<       ohm,   "Ω", su::scale<>,    mass<>,   length<2>,    time<-3>, current<-2> >{};
struct   siemens : su::symbol_unit<   siemens,   "S", su::scale<>,  mass<-1>,  length<-2>,     time<3>,  current<2> >{};
struct     weber : su::symbol_unit<     weber,  "Wb", su::scale<>,    mass<>,   length<2>,    time<-2>, current<-1> >{}; 
struct     henry : su::symbol_unit<     henry,   "H", su::scale<>,    mass<>,   length<2>,    time<-2>, current<-2> >{};

struct     lumen : su::symbol_unit<     lumen,  "lm", su::scale<>,  angle<2>, luminous_intensity<>             >{};
struct       lux : su::symbol_unit<       lux,  "lx", su::scale<>,  angle<2>, luminous_intensity<>, length<-2> >{};

// Non SI (approved) named;

struct    minute : su::scaled_unit< minute, "min",  su::scale<60>, second >{};
struct      hour : su::scaled_unit<   hour,   "h",  su::scale<60>, minute >{};
struct       day : su::scaled_unit<    day,   "d",  su::scale<24>,   hour >{};
struct      year : su::scaled_unit<   year,  "yr", su::scale<365>,    day >{};

namespace prefix {

using yotta = su::scale<24>;
using zetta = su::scale<21>;
using   exa = su::scale<18>;
using  peta = su::scale<15>;
using  tera = su::scale<12>;
using  giga = su::scale<9>;
using  mega = su::scale<6>;
using  kilo = su::scale<3>;
using hecto = su::scale<2>;
using  deka = su::scale<1>;
using  deci = su::scale<-1>;
using centi = su::scale<-2>;
using milli = su::scale<-3>;
using micro = su::scale<-6>;
using  nano = su::scale<-9>;
using  pico = su::scale<-12>;
using femto = su::scale<-15>;
using  atto = su::scale<-18>;
using zepto = su::scale<-21>;
using yocto = su::scale<-24>;

using  kibi = su::scale_make<1, 1024, 1>;
using  mebi = su::scale_multiply_t<kibi, kibi>;
using  gibi = su::scale_multiply_t<mebi, kibi>;
using  tebi = su::scale_multiply_t<gibi, kibi>;
using  pebi = su::scale_multiply_t<tebi, kibi>;
using  exbi = su::scale_multiply_t<pebi, kibi>;
using  zebi = su::scale_multiply_t<exbi, kibi>;
using  yobi = su::scale_multiply_t<zebi, kibi>;

} // namespace prefix

} // namespace si



