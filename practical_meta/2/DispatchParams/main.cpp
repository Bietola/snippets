#include <iostream>
#include <functional>
#include <tuple>

template <typename Function,
          typename... Params,
          size_t... I>
void dispatch_params_impl(const Function& func,
                          const std::tuple<Params...>& params,
                          const std::index_sequence<I...>) {
    func(std::get<I>(params)...);
}

template <typename Function,
          typename... Params>
void dispatch_params(const Function& func,
                     const std::tuple<Params...>& params) {
    dispatch_params_impl(func, params,
                         std::make_index_sequence<sizeof...(Params)>());
}

int do_thing(const std::string& thing) {
    std::cout << "I've done the " << thing << " sir/ma'am!";
}

int main() {
    dispatch_params(&do_thing, std::make_tuple(std::string("bath")));

    return 0;
}
