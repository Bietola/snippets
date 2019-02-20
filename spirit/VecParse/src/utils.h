#pragma once 

#include <vector>
#include <type_traits>

template <class V>
void vec_print(const V& v) {
    if constexpr (std::is_same_v<std::vector<std::vector<double>>, V>) {
        for (const auto& row : v) {
            for (const auto& ele : row) {
                std::cout << ele << ", ";
            }
            std::cout << "\n";
        }
    }
    else {
        for (const auto& ele : v) {
            std::cout << ele << ", ";
        }
    }
    std::cout << "\n";
}
