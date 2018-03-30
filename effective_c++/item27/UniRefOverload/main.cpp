#include <iostream>
#include <vector>
#include <string>

#include "metautils.h"

class Person {
    private:
        std::string mName;
        size_t mId = 0;

    public:
        explicit Person(size_t id, std::string name):
            mId(id),
            mName(std::move(name)){}; 
        Person(const Person& cp): mName(cp.mName) {
            std::cout << "Person(Person const&)\n";
        }
        Person(const Person&& mv): mName(std::move(mv.mName)) {
            std::cout << "Person(Person &&)\n";
        }

        const std::string& getName() const {return mName;}
};

std::vector<Person> gNames;

template <typename... Args>
void subscribe_impl(const std::tuple<Args...>& args) {
    auto name = find_tuple_type_t<std::string>(args);
    std::cout << "registered " << name << '\n';
    gNames.emplace_back(std::forward<Args>(args)...);
}

template <typename... Args>
void subscribe(Args&&... args) {
    subscribe_impl(std::make_tuple(std::forward<Args>(args)...));
}

int main() {
    subscribe(std::string("Bob"), 1);

    return 0;
}
