#include <iostream>

template <typename T>
auto do_thing(T&& thing) {
    std::cout << __PRETTY_FUNCTION__ << std::endl;
    return thing;
}

int main() {
    auto res = do_thing(10);
    res = do_thing(res);
    std::cout << typeid(res).name() << std::endl;

    return 0;
}
