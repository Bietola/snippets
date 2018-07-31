#include <iostream>

#include "utils.h"

class Greeter :
    public utl::clonable<Greeter>
{
    public:
        virtual ~Greeter() {
            std::cout << "destroyed!" << std::endl;
        }

        virtual std::string greet() const = 0;
};

class WorldGreeter :
    public utl::clone_inherit<Greeter, WorldGreeter>
{
    public:
        std::string greet() const override { return "hello world!"; }
};

int main() {
    std::unique_ptr<Greeter> greeter1 = std::make_unique<WorldGreeter>();
    auto greeter2 = greeter1->clone();

    std::cout << greeter1->greet() << "\n";
    std::cout << greeter2->greet() << "\n";

    return 0;
}
