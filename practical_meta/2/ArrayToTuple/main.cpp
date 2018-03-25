#include <iostream>
#include <tuple>
#include <array>


template <typename T, size_t size,
          size_t... I>
auto a2t_impl(const std::array<T, size>& arr,
              std::index_sequence<I...>) {
    return std::make_tuple(arr[I]...);
}

template <typename T, size_t size,
          typename Indicies = std::make_index_sequence<size>>
auto a2t(const std::array<T, size>& arr) {
    return a2t_impl(arr, Indicies());
}

template <typename Tuple, typename Function,
          size_t... I>
void for_tuple_impl(const Tuple& tpl,
                    const Function& func,
                    std::index_sequence<I...>) {
    func(std::get<I>(tpl))...;
}

template <typename Tuple, typename Function,
          typename Indices = std::make_index_sequence(
              std::tuple_size_v<Tuple>
          )>
void for_tuple(const Tuple& tpl,
               const Function& func) {
    for_tuple_impl(tpl, func, Indicies{});
}


int main() {
    std::array<int, 5> arr = {1, 2, 3, 4, 5};

    auto tpl = a2t(arr);

    for_tuple(tpl, [] (const auto& ele) {
        std::cout << ele << ", ";
    }});    
    std::cout << std::endl;

    return 0;
}
