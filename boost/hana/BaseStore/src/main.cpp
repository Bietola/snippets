#include <iostream>

#include "utils.h"

class Greeter :
    public clonable<Greeter> 
{
    public:
        virtual ~Greeter() {
            std::cout << "destroyed!" << std::endl;
        }

        virtual std::string greet() const = 0;
};

class WorldGreeter :
    public Greeter
{
    public:
        std::string greet() const override { return "hello world!"; }
};

int main() {
    Greeter* greeter1  = new WorldGreeter;
    Greeter* greeter2 = greeter1->clone();

    std::cout << greeter1->greet() << "\n";
    std::cout << greeter2->greet() << "\n";

    delete greeter1;
    delete greeter2;

    return 0;
}
