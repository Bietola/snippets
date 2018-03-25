#include <iostream>
#include <tuple>

template <typename>
struct make_tuple_of_params;

template <typename Func, typename... Args>
struct make_tuple_of_params<Func(Args...)> {
    using type = std::tuple<Args...>;
};

template <typename T>
using make_tuple_of_params_t = typename make_tuple_of_params<T>::type;

void do_thing(const std::string& thing, size_t amount) {
    std::cout << "I did " << thing << " " << amount << " times, sir/ma'am!\n";
}


int main() {
    using tpl_type = make_tuple_of_params_t<decltype(do_thing)>;
    tpl_type tpl = {std::string("bath"), 10};

    do_thing(std::get<0>(tpl), std::get<1>(tpl));
    
    return 0;
}

