#include <iostream>

#include "utils.h"

class Greeter:
    public utl::clonable<Greeter, std::unique_ptr>
{
    public:
        virtual ~Greeter() { std::cout << "deleted something!\n"; }

        virtual std::string greet(const std::string&) const = 0;
};

class WorldGreeter :
    public utl::clone_inherit<Greeter, WorldGreeter>
{
    public:
        std::string greet(const std::string& name) const override { return "hello " + name + "!!!"; }
};

int main() {
    Greeter::BasePtr greeter1 = Greeter::make_ptr<WorldGreeter>();
    auto greeter2 = greeter1->clone();

    std::cout << greeter1->greet("world") << std::endl;
    std::cout << greeter2->greet("world") << std::endl;

    return 0;
}
