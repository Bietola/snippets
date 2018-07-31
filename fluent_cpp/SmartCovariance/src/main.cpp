#include <iostream>

#include "utils.h"

class Greeter : 
    public utl::cloneable<Greeter, std::unique_ptr> {

    public:
        virtual std::string greet(const std::string&) const = 0;
};

class WorldGreeter : public utl::clone_inherit<WorldGreeter, Greeter> {
    public:
        std::string greet(const std::string& toGreet) const override {
            return "hello " + toGreet + "!!!";
        }
};

class GrumpyGreeter : public utl::clone_inherit<GrumpyGreeter, Greeter> {
    public: 
        std::string greet(const std::string& toGreet) const override {
            return "hello " + toGreet + "...";
        }
};

int main() {
    auto greeter1 = std::make_unique<WorldGreeter>();
    auto greeter2 = greeter1->clone();

    std::cout << "1: " << greeter1->greet("world") << "\n";
    std::cout << "2: " << greeter2->greet("world") << "\n";

    return 0;
}
