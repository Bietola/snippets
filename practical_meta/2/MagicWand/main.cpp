#include <iostream>
#include <tuple>

template <typename T, typename... Funcs,
          size_t... I>
auto reverse_apply(T&& value, const std::tuple<Funcs...>& funcs,
                   std::index_sequence<I...>) {
    return std::make_tuple(std::get<I>(funcs)(std::forward<T>(value))...);
}

template <typename T, typename... Funcs>
auto reverse_apply(T&& value, const std::tuple<Funcs...>& funcs) {
    return reverse_apply(std::forward<T>(value), funcs,
                         std::make_index_sequence<sizeof...(Funcs)>());
}

template <typename... Args,
          typename GetterFunc, typename OldFunc,
          size_t... I>
auto magic_wand(const std::tuple<Args...>& args,
                GetterFunc&& alpha_getter, GetterFunc&& beta_getter,
                OldFunc&& old_function,
                std::index_sequence<I...>) {
    auto ret_vals = std::tuple_cat(
        reverse_apply(std::get<I>(args), std::make_tuple(alpha_getter, beta_getter))...
    );

    std::apply(old_function, ret_vals);

    return ret_vals;
}

int main() {
    auto print = [] (auto&& thing){
        std::cout << thing << std::endl;
    };
    auto print_more = [] (auto&& thing) {
        std::cout << "even more of " << thing << std::endl;
    };


    reverse_apply("bisquit", std::make_tuple(print, print_more));

    return 0;
}
