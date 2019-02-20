#include <iostream>

#include <boost/spirit/include/qi.hpp>

#include "utils.h"

#define ALL(RNG) std::begin(RNG), std::end(RNG)

namespace alst {

using namespace boost::spirit;

using out_type = std::vector<std::vector<double>>;

template <class Iterator>
bool parse(Iterator start, Iterator end,
           out_type& out) {
    using qi::double_;

    /* auto list_  = double_ % ','; */
    /* auto expr_  = '[' >> list_ >> ']'; */
    /* auto stmnt_ = expr_ >> '.'; */
    /* auto prog_  = +stmnt_; */

    bool res = qi::phrase_parse(
        start,
        end,
        +('[' >> (double_ % ',') >> ']'),
        qi::space,
        out
    );

    return (start == end) && res;
}

}

int main() {
    std::string input = " \
        [1, 2, 3] \
        [4, 5, 6] \
    ";

    alst::out_type output;
    std::cout <<
        (alst::parse(ALL(input), output) ?
            "success!" :
            "failure..."
        ) << "\n";

    vec_print(output);

    return 0;
}
