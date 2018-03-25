#include <iostream>
#include <functional>
#include <tuple>

template <typename... Funcs, typename... Params,
          size_t... FuncIndex, size_t... ParamIndex>
auto dispatch_functions_impl(const std::tuple<Funcs...>& funcs,
                             const std::tuple<Params...>& params,
                             std::index_sequence<FuncIndex...>,
                             std::index_sequence<ParamIndex...>) {
    return std::make_tuple(
        std::get<FuncIndex>(funcs)(std::get<ParamIndex>(params))...
    );
}

template <typename... Funcs, typename... Params>
auto dispatch_functions(const std::tuple<Funcs...>& funcs,
                        const std::tuple<Params...>& params) {
    return dispatch_functions_impl(funcs, params,
                                   std::make_index_sequence<sizeof...(Funcs)>(),
                                   std::make_index_sequence<sizeof...(Params)>());
}


int main() {
    auto funks = std::make_tuple(
        [] (auto& val) {val = 1; return 0;},
        [] (auto& val) {val = 2; return 0;},
        [] (auto& val) {val = 3; return 0;}
    );

    int x1 = 0, x2 = 0, x3 = 0;

    auto params = std::make_tuple(std::ref(x1), std::ref(x2), std::ref(x3));

    dispatch_functions(funks, params);

    std::cout << x1 << ", " << x2 << ", " << x3 << '\n';

    return 0;
}
