#include <iostream>

#include <boost/spirit/include/qi.hpp>

using namespace boost::spirit::qi;

#define ALL(RNG) std::begin(RNG), std::end(RNG)

template <class Iterator>
bool parse_numbers(Iterator first, Iterator last) {
    bool res = phrase_parse(
        first,
        last,
        double_ >> *((char_(',') | char_(';'))  >> double_),
        space
    );

    if (first != last)
        return false;
    return res;
}

int main() {
    std::string numbers = "1, 2, 3, 4; 5";

    std::cout << parse_numbers(ALL(numbers)) << '\n';

    return 0;
}
