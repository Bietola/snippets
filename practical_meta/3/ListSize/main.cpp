#include <iostream>
#include <tuple>

template <size_t idx>
using index_constant = std::integral_constant<size_t, idx>;

template <class> struct size;

template <template <class...> class List, class... Elements>
struct size<List<Elements...>> : index_constant<sizeof...(Elements)> {};

template <class List>
size_t size_v = size<List>::value;

int main() {
    std::cout << size_v<std::tuple<void, void, void>> << std::endl;

    return 0;
}
