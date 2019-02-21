#include <iostream>

#include <boost/spirit/include/qi.hpp>

using namespace boost::spirit;

#define ALL(RNG) std::begin(RNG), std::end(RNG)

template <class Iterator>
class roman : qi::grammar<Iterator, unsigned> {
    public:
        struct hundreds_ : qi::symbols<char, unsigned> {
            hundreds_() {
                add
                    ("C"    , 100u)
                    ("CC"   , 200u)
                    ("CCC"  , 300u)
                    ("CD"   , 400u)
                    ("D"    , 500u)
                    ("DC"   , 600u)
                    ("DCC"  , 700u)
                    ("DCCC" , 800u)
                    ("CM"   , 900u)
                ;
            }
        } hundreds;

        struct tens_ : qi::symbols<char, unsigned> {
            tens_() {
                add
                    ("X"    , 10u)
                    ("XX"   , 20u)
                    ("XXX"  , 30u)
                    ("XL"   , 40u)
                    ("L"    , 50u)
                    ("LX"   , 60u)
                    ("LXX"  , 70u)
                    ("LXXX" , 80u)
                    ("XC"   , 90u);
            }
        } tens;

        struct ones_ : qi::symbols<char, unsigned> {
            ones_() {
                add
                    ("I"    , 1u)
                    ("II"   , 2u)
                    ("III"  , 3u)
                    ("IV"   , 4u)
                    ("V"    , 5u)
                    ("VI"   , 6u)
                    ("VII"  , 7u)
                    ("VIII" , 8u)
                    ("IX"   , 9u);
            }
        } ones;

        roman() : roman::base_class(start) {
            using qi::lit;
            using qi::eps;
            using qi::_val;
            using qi::_1;

            start =
                eps             [_val = 0] >>
                (
                    +lit('M')   [_val += 1000u]
                    || hundreds [_val += _1]
                    || tens     [_val += _1]
                    || ones     [_val += _1]
                );
        }

    private:
        qi::rule<Iterator, unsigned()> start;
};

int main() {
    std::cout << "hello world\n";

    return 0;
}
