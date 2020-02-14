#include <iostream>

#include "si.hpp"

using namespace si;

namespace unit {

template <typename S, typename... Dims>
auto crazy(list<S, Dims...>) {
    return;
}

template <template <typename> typename CRTP>
struct Pass {};

template <template <typename> typename CRTP, fs::fixed_string Sym, Arithmetic T,
          Scale S, Dimension... Dims>

struct named_unit : unit_make<T, S, Dims...> {
    using unit_make<T, S, Dims...>::unit;
    using unit_make<T, S, Dims...>::operator=;

    [[nodiscard]] inline static constexpr std::string_view symbol() noexcept {
        return m_symbol.view();
    }

   private:
    friend auto crazy(list<S, Dims...>) { return Pass<CRTP>{}; }

    static constexpr fs::fixed_string m_symbol = Sym;
};

template <typename T>
struct conor : named_unit<conor, "conors", double, scale<>, length<>> {
    using named_unit<conor, "conors", double, scale<>, length<>>::named_unit;
    using named_unit<conor, "conors", double, scale<>, length<>>::operator=;
};

template <typename T>
struct finlay : named_unit<finlay, "finlay", double, scale<>, si::time<>> {
    using named_unit<finlay, "finlay", double, scale<>, si::time<>>::named_unit;
    using named_unit<finlay, "finlay", double, scale<>, si::time<>>::operator=;
};

template <typename, typename>
struct expand;

template <template <typename> typename CRTP, typename>
struct expand<Pass<CRTP>>;

template <Unit A, Unit B>
constexpr auto sum(A lhs, B rhs) requires dimension_equal_v<A, B> {
    using product_t = decltype(lhs.get() + raw_convert<A>(rhs));

    using dim_type = decltype(
        crazy(list<typename A::scale_factor, typename A::dimensions>{}));

    using unit_t =
        unit_make_from_sorted_t<decltype(lhs.get() + raw_convert<A>(rhs)),
                                typename A::scale_factor,
                                typename A::dimensions>;

    return unit_t{lhs.get() + raw_convert<A>(rhs)};
}

}  // namespace unit

using namespace unit;

int main() {
    conor<int> t{1.};

    t = meter;

    std::cout << sum(t, t) << ' ' << t.symbol() << std::endl;

    return 0;
}
