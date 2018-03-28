#include <iostream>
#include <type_traits>

/***********************************/
/* custom enable if implementation */
/***********************************/
template <bool, typename T = void>
struct enable_if {
    using type = T;
};

template <typename T>
struct enable_if<false, T> {};

template <bool C, typename T>
using enable_if_t = typename enable_if<C, T>::type;


/***********************************************/
/* shitty precision-accounting equals function */
/***********************************************/
// integral types
template <typename Rhs, typename Lhs>
enable_if_t<
    std::is_integral_v<
        std::common_type_t<Rhs, Lhs>>, bool>
equals(Rhs rhs, Lhs lhs) {
    return rhs == lhs;
}

// floating point types
template <typename Rhs, typename Lhs>
enable_if_t<
    std::is_floating_point_v<
        std::common_type_t<Rhs, Lhs>>, bool>
equals(Rhs rhs, Lhs lhs) {
    static const double delta = 0.01;
    return std::abs(rhs - lhs) < delta;
}

// MAIN
int main() {
    std::cout << equals(10.00001 - 9, 1) << std::endl;

    return 0;
}
