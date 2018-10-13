#include <iostream>
#include <type_traits>

template <class T, class = void, class... Args>
struct defrangulate : std::false_type {};

template <class... Args>
struct defrangulate<int,
                    std::enable_if_t<
                        std::conjunction_v<std::is_void<Args>...>
                    >,
                    Args...> : std::true_type {};

template <class T, class... Args>
bool defrangulate_v = defrangulate<T, void, Args...>::value;

int main() {
    std::cout << defrangulate_v<int, const void, const void, volatile void, const volatile void> << "\n";

    return 0;
}
