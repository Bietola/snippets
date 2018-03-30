#include <cassert>
#include <iostream>
#include <tuple>

template <typename LegacyFunc, typename... Params>
auto dispatchToLegacy(const LegacyFunc& legacyFunc,
                      Params&&... params) {
    legacyFunc(&params...);
    return std::make_tuple(std::forward<Params>(params)...);
}

void legacyFunk(double* a, int* b, const char** c) {
    assert(a && b && c);

    *a = 1.4;
    *b = 3;
    *c = "hello there!";
}

int main() {
    double a;
    int b;
    const char* c;
    auto ret = dispatchToLegacy(legacyFunk, a, b, c);
    std::cout << std::get<2>(ret) << std::endl;
    
    return 0;
}
