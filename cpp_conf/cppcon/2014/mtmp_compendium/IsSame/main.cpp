#include <iostream>

struct Bob {
    static constexpr const char* name = "bob";
};

template <typename T>
struct is_bob : std::is_same<T, Bob> {};

template <typename T>
constexpr bool is_bob_v = is_bob<T>::value;

int main() {
    Bob bob;

    if constexpr(is_bob_v<decltype(bob)>) {
        std::cout << "I found bob..." << std::endl;
    }
    else {
        std::cout << "Turns out bob is no bob..." << std::endl;
    }

    return 0;
}
