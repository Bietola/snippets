#pragma once

#include <string>

auto greet(const std::string& noun) {
    return std::string("hello there ") + noun + std::string("!!!");
}
