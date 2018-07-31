#include <iostream>

#include <boost/hana.hpp>

namespace hana = boost::hana;
using namespace hana::literals;

struct Cow {
    std::string name;
};

struct Crow {
    std::string name;
};

struct Cuttlefish {
    std::string name;
};

int main() {
    // fun with animals
    /* auto animals = hana::make_tuple(Cow{"cow"}, Crow{"crow"}, Cuttlefish{"fish"}); */
    /* auto names = hana::transform(animals, [] (const auto& ele) { */
    /*     return ele.name; */
    /* }); */
    /* hana::for_each( */
    /*     hana::transform( */
    /*         hana::reverse(animals), */
    /*         [] (const auto& ele) { */
    /*             return ele.name; */
    /*         } */
    /*     ), */
    /*     [] (const auto& ele) { */
    /*         std::cout << ele << std::endl; */
    /*     } */
    /* ); */

    // fun with animal types
    /* auto animalTypes = hana::make_tuple(hana::type<Cow>(), hana::type<Crow>(), hana::type<Cuttlefish>()); */
    /* auto animalPtrs  = hana::transform( */
    /*     animalTypes, */
    /*     [] (const auto& ele) { */
    /*         return (ele); */
    /*     } */
    /* ); */

    return 0;
}
