#include <iostream>

struct Thing {
    /* Thing& operator=(const Thing&) { */
    /*     std::cout << "done..." << std::endl; */
    /* } */

    Thing& operator=(const Thing&) = delete;
};

template <class T>
using copy_assignment_t = decltype(std::declval<T&>() = std::declval<const T&>());

template <class, class = void>
struct is_copy_assignable : std::false_type {};

template <class T>
struct is_copy_assignable<T, std::void_t<copy_assignment_t<T>>> : 
    std::is_same<copy_assignment_t<T>, T&> {};

template <class T>
bool is_copy_assignable_v = is_copy_assignable<T>::value;

int main() {
    std::cout << is_copy_assignable_v<Thing> << std::endl;

    return 0;
}
