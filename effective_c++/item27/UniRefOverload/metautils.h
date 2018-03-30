#pragma once

template <size_t idx>
using index_constant = std::integral_constant<size_t, idx>;

template <size_t idx, typename, typename...>
struct find_var_impl {};

template <size_t idx, typename T, typename... List>
struct find_var_impl<idx, T, T, List...> : index_constant<idx> {};

template <size_t idx, typename T, typename U, typename... List>
struct find_var_impl<idx, T, U, List...> :
    find_var_impl<idx + 1, T, List...> {};

template <typename T, typename... List>
struct find_var :
    find_var_impl<0, T, List...> {};

template <typename T, typename... List>
size_t find_var_v = find_var<T, List...>::value;
