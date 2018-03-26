#include <iostream>
#include <tuple>
#include <functional>
#include <sstream>

template <typename T>
decltype(auto) identity(T&& thing) {
    return std::forward<T>(thing);
}

template <typename T>
std::string to_string(const T& thing) {
    std::stringstream ss;
    ss << thing;
    return ss.str();
}

template <typename... Params, typename Predicate,
          size_t... I>
void print_tuple_impl(const std::tuple<Params...>& tpl,
                      const Predicate& predicate,
                      std::index_sequence<I...>) {
    (std::cout << ... << predicate(std::get<I>(tpl)));
}

template <typename... Params,
          typename Predicate>
void print_tuple(const std::tuple<Params...>& tpl,
                 const Predicate& predicate = identity) {
    print_tuple_impl(tpl, predicate, std::make_index_sequence<sizeof...(Params)>());
}

int main() {
    auto tpl = std::make_tuple(1, 2, 3, "hello!");
    /* print_tuple(tpl, [] (const auto& ele) { */
    /*     return to_string(ele) + to_string(", "); */
    /* }); */
    print_tuple(tpl);

    return 0;
}
