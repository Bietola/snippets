#include <iostream>

template <typename T>
struct rank {
    static constexpr const size_t value = 0;
};

template <typename T,
          size_t S>
struct rank<T[S]> {
    static constexpr const size_t value = 1 + rank<T>::value;
};

template <typename T>
static constexpr const size_t rank_v = rank<T>::value;

int main() {
    int x[10][10][10][10][10][10];
    std::cout << rank_v<decltype(x)> << std::endl;

    return 0;
}
