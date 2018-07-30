#include <iostream>
#include <string>
#include <memory>

#include "utils.h"

struct Interface :
    public utl::clonable<Interface, std::unique_ptr> {

    std::string mAdjective;

    void setAdjective(const std::string& adj) { mAdjective = adj; }

    virtual std::string greet(const std::string&) const = 0;
};

struct Implementation :
    public utl::clonable_impl<Interface, Implementation, std::unique_ptr> {

    std::string greet(const std::string& str) const override { return "hello " + str + "!!!"; }
};

int main() {
    auto greeter = Interface::make<Implementation>();
    std::cout << "test1" << std::endl;
    std::cout << greeter->greet("world") << std::endl << std::endl;

    auto greeter2 = greeter->clone();
    greeter2->setAdjective("cruel");
    std::cout << "test2" << std::endl;
    std::cout << greeter ->greet("world") << std::endl;
    std::cout << greeter2->greet("world") << std::endl;

    return 0;
}
