#include <iostream>
#include <tuple>

template <typename Function, typename... Params,
          size_t... I>
void dispatch_params(Function func,
                     const std::tuple<Params...>& params,
                     std::index_sequence<I...>) {
    func(std::get<I>(params)...);
}

template <typename Function, typename... Params>
void dispatch_params(Function func,
                     const std::tuple<Params...>& params) {
    dispatch_params(func, params, std::make_index_sequence<sizeof...(Params)>());
}

template <typename... Ints>
void print_ints(Ints... ints) {
    std::cout << "Here's your ints, sir/ma'am: ";
    (std::cout << ints... << ", ");
}

int main() {
    auto tpl = std::make_tuple(1, 3, 24);

    return 0;
}
