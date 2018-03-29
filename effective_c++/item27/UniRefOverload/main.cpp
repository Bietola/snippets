#include <iostream>
#include <vector>
#include <string>

class Person {
    private:
        std::string mName;
        size_t mId = 0;

    public:
        explicit Person(const std::string& name, size_t id):
            mName(name),
            mId(id){}; 
        Person(const Person& cp): mName(cp.mName) {
            std::cout << "Person(Person const&)\n";
        }
        Person(const Person&& mv): mName(std::move(mv.mName)) {
            std::cout << "Person(Person &&)\n";
        }

        const std::string& getName() const {return mName;}
};

std::vector<Person> gNames;

template <typename Name, typename... Args>
void subscribe(Name&& name, Args&&... args) {
    std::cout << "registered " << name << '\n';
    gNames.emplace_back(std::forward<Name>(name),
                        std::forward<Args>(args)...);
}

int main() {
    subscribe("Bob", 1);

    return 0;
}
