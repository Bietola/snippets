#include <iostream>
#include <functional>

template <typename Signature>
struct func_traits_impl {};

template <typename Ret, typename... Args>
struct func_traits_impl<Ret(Args...)> {
    using return_type = Ret;
    static constexpr size_t arity = sizeof...(Args);
};

template <typename Sig>
struct func_traits_impl<std::function<Sig>> :
    func_traits_impl<Sig> {};

template <typename Func>
struct func_traits :
    func_traits_impl<decltype(std::function{std::declval<Func>()})> {};

int main() { 
    auto funk = [] (int times, int grandmas) {
        return std::string("hello there ") + std::to_string(times) + " times to your " +
            std::to_string(grandmas) + " grandmas!!!";
    };

    using funk_t = decltype(funk);

    func_traits<decltype(funk)>::return_type arity_intro = "here's your arity, sir/ma'am: ";

    std::cout << arity_intro << func_traits<funk_t>::arity << ".\n";

    return 0;
}
