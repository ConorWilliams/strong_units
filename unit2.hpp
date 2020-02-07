#include <cstdint>
#include <ratio>
#include <stdexcept>
#include <type_traits>

template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

///////////////////////////// STR ///////////////////////////////

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

///////////////////////////// SCALE //////////////////////////////////

template <typename T>
concept Scale = requires {
    typename T::fact;
    typename T::ofst;

    T::fact::num;
    T::fact::den;

    T::ofst::num;
    T::ofst::den;
};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct ScaleBase {
    using fact = std::ratio<I, J>;
    using ofst = std::ratio<K, L>;

    static_assert(fact::num != 0, "Cannot have a zero scaled dimension");
};

template <std::intmax_t... Is>
struct scale;

template <>
struct scale<> : ScaleBase<1, 1, 0, 1> {};

template <std::intmax_t I>
struct scale<I> : ScaleBase<I, 1, 0, 1> {};

template <std::intmax_t I, std::intmax_t J>
struct scale<I, J> : ScaleBase<I, J, 0, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K>
struct scale<I, J, K> : ScaleBase<I, J, K, 1> {};

template <std::intmax_t I, std::intmax_t J, std::intmax_t K, std::intmax_t L>
struct scale<I, J, K, L> : ScaleBase<I, J, K, L> {};

namespace detail {

template <typename, typename>
struct scale_equal : std::false_type {};

template <std::intmax_t... Il, std::intmax_t... Ir>
struct scale_equal<scale<Il...>, scale<Ir...>> {
    static constexpr bool value =
        std::ratio_equal_v<typename scale<Il...>::fact,
                           typename scale<Ir...>::fact> &&

        std::ratio_equal_v<typename scale<Il...>::ofst,
                           typename scale<Ir...>::ofst>;
};

}  // namespace detail

template <Scale A, Scale B>
inline constexpr bool scale_equal_v = detail::scale_equal<A, B>::value;

// value_b = s::fact * value_s + s::ofst
// value_b = k::fact * value_k + k::ofst
//
// k::fact * value_k + k::ofst = s::fact * value_s + s::ofst
// k::fact * value_k = s::fact * value_s + (s::ofst - k::ofst)
//
// value_k = s::fact / k::fact * value_s + (s::ofst - k::ofst) / k::fact
//
// value_t = f::fact / t::fact * value_f + (f::ofst - t::ofst) / t::fact
//
// Could be generalised to arbitrary scale<...> !

template <Scale From, Scale To, Arithmetic T>
inline constexpr T convert(T x) {
    using fact = std::ratio_divide<typename From::fact, typename To::fact>;

    using ofst = std::ratio_divide<
        std::ratio_subtract<typename From::ofst, typename To::ofst>,
        typename To::fact>;

    if constexpr (std::ratio_equal_v<ofst, std::ratio<0>>) {
        if constexpr (std::ratio_equal_v<fact, std::ratio<0>>) {
            return x;
        } else {
            return static_cast<T>(fact::num) / fact::den * x;
        }
    } else {
        if constexpr (std::ratio_equal_v<fact, std::ratio<0>>) {
            return x + static_cast<T>(ofst::num) / ofst::den;
        } else {
            return static_cast<T>(fact::num) / fact::den * x +
                   static_cast<T>(ofst::num) / ofst::den;
        }
    }

    // return std::ratio_divide<typename _R1, typename _R2>
}

//////////////////////////////   Dimension ////////////////////////////////////

template <std::intmax_t... Is>
struct DimensionBase;

template <>
struct DimensionBase<> {
    using exp = std::ratio<1, 1>;
};

template <std::intmax_t I>
struct DimensionBase<I> {
    using exp = std::ratio<I, 1>;
};

template <std::intmax_t I, std::intmax_t J>
struct DimensionBase<I, J> {
    using exp = std::ratio<I, J>;
};

template <typename T>
concept Dimension = requires {
    typename T::exp;
    T::symbol;
};

/////////////////////////// SI base units   ////////////////////////////

namespace si {

template <std::intmax_t... Is>
struct meter : DimensionBase<Is...> {
    static constexpr str_const symbol = "m";
};

template <std::intmax_t... Is>
struct second : DimensionBase<Is...> {
    static constexpr str_const symbol = "s";
};

template <std::intmax_t... Is>
struct kilogram : DimensionBase<Is...> {
    static constexpr str_const symbol = "kg";
};

template <std::intmax_t... Is>
struct ampere : DimensionBase<Is...> {
    static constexpr str_const symbol = "A";
};

}  // namespace si

//////////////////////////// UNIT  //////////////////////////////

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

// value_in_base = s::fact * value + s::ofst
template <Arithmetic T, Scale S, Dimension... Dims>
    requires ordered(Dims{}...) && (... && (Dims::exp::num != 0)) struct unit {
    using value_type = T;

    value_type value;

    inline constexpr value_type get() const { return value; }
};

template <typename>
struct is_unit : std::false_type {};

template <Arithmetic T, Scale S, Dimension... Dims>
struct is_unit<unit<T, S, Dims...>> : std::true_type {};

template <typename T>
concept Unit = is_unit<T>::value;

///////////////////////////  operators ////////////////////////////

// template <typename, typename>
// struct equivilant : std::false_type {};

// template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
//           std::intmax_t... Ir>
// struct equivilant<Dim<Il...>, Dim<Ir...>> {
//     static constexpr bool value =
//         std::ratio_equal_v<typename Dim<Il...>::exp, typename
//         Dim<Ir...>::exp>;
// };

template <typename, typename>
struct same_dimension : std::false_type {};

template <template <std::intmax_t...> typename Dim, std::intmax_t... Il,
          std::intmax_t... Ir>
struct same_dimension<Dim<Il...>, Dim<Ir...>> {
    static constexpr bool value = true;
};

template <Dimension A, Dimension B>
struct equivilant {
    static constexpr bool value =
        same_dimension<A, B>::value &&
        std::ratio_equal_v<typename A::exp, typename B::exp>;
};

template <Arithmetic Tl, Scale Sl, Dimension... Dl, Arithmetic Tr, Scale Sr,
          Dimension... Dr>
constexpr inline auto operator+(
    unit<Tl, Sl, Dl...> lhs,
    unit<Tr, Sr, Dr...>
        rhs) requires std::conjunction_v<equivilant<Dl, Dr>...> {
    if constexpr (scale_equal_v<Sl, Sr>) {
        return unit<decltype(lhs.get() + rhs.get()), Sl, Dl...>{lhs.get() +
                                                                rhs.get()};
    } else {
        return unit<decltype(lhs.get() + rhs.get()), Sl, Dl...>{
            lhs.get() + convert<Sr, Sl>(rhs.get())};
    }
}

// (A B C D, C D E F) -> (A B, C D, E F)
// sum  middle
// concat the 3 list
