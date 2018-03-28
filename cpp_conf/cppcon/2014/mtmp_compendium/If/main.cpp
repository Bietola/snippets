#include <iostream>

template <typename T>
struct type_is {
    using type = T;
};

template <bool, typename T, typename F>
struct IF : type_is<F> {};

template <typename T, typename F>
struct IF<true, T, F> : type_is<T> {};

template <bool C, typename T, typename F>
using IF_t = typename IF<C, T, F>::type;

int main() {
    IF_t<true, int, float> x = 1.1;

    std::cout << x << std::endl;

    return 0;
}
