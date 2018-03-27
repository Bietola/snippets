#include <iostream>

template <typename T>
struct type_is {
    using type = T;
};

template <bool, typename T, typename>
struct IF : type_is<T> {};

template <typename T, typename F>
struct IF<false, T, F> : type_is<F> {};

template <bool Cond, typename Ttrue, typename Tfalse>
using IF_t = typename IF<Cond, Ttrue, Tfalse>::type;

int main() {
    std::cout <<
        IF_t<true, int, double>(1.1) << std::endl;

    return 0;
}
