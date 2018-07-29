#include <iostream>
#include <string>
#include <memory>

struct Interface {
    template <class T> using Ptr = std::unique_ptr<T>;

    std::string mAdjective;

    void setAdjective(const std::string& adj) { mAdjective = adj; }

    virtual std::string greet(const std::string&) const = 0;
    virtual Interface* clone() const = 0;
};

struct Implementation : public Interface {
    std::string greet(const std::string& str) const override { return "hello " + str + "!!!"; }

    Implementation* clone() const override { return new Implementation(*this); }
};

int main() {
    Interface* greeter = new Implementation;
    std::cout << "test1" << std::endl;
    std::cout << greeter->greet("world") << std::endl << std::endl;

    Interface* greeter2 = greeter->clone();
    greeter2->setAdjective("cruel");
    std::cout << "test2" << std::endl;
    std::cout << greeter ->greet("world") << std::endl;
    std::cout << greeter2->greet("world") << std::endl;

    return 0;
}
