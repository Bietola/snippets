#include <iostream>

#include "utils.h"

class Greeter : public utl::cloneable<Greeter> {
    public:
        template <class Derived> 
        struct Deleter {
            void operator()(Derived* ptr) {
                std::cout << "deleted something unique (" << typeid(ptr).name() << ")" << std::endl;
                delete ptr;
            }
        };

    protected:
        std::string mAdjective;

    public:
        virtual ~Greeter() {
            std::cout << "deleted something (" << typeid(this).name() << ")"
                      << " [" << this << "]" << std::endl;
        }

        void setAdjective(const std::string& adj) { mAdjective = adj; }

        virtual std::string greet(const std::string&) const = 0;
};


template <class Derived> 
using GreeterImpl = utl::clone_inherit<
    Derived, Greeter, std::unique_ptr, Greeter::Deleter
>;

class WorldGreeter : public GreeterImpl<WorldGreeter> {
    public:
        std::string greet(const std::string& toGreet) const override {
            return "hello " + mAdjective + " " + toGreet + "!!!";
        }
};

class GrumpyGreeter : public GreeterImpl<GrumpyGreeter> {
    public: 
        std::string greet(const std::string& toGreet) const override {
            return "hello " + mAdjective + " " + toGreet + "...";
        }
};

int main() {
    auto greeter1 = std::make_unique<WorldGreeter>();
    greeter1->setAdjective("cruel");

    auto greeter2 = greeter1->clone();
    greeter2->setAdjective("beautiful");

    std::cout << "1: " << greeter1->greet("world") << "\n";
    std::cout << "2: " << greeter2->greet("world") << "\n";

    return 0;
}
