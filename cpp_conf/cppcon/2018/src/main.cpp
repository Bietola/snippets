#include <iostream>

#include "fsm.h"

int main() {
    Fsm fsm{std::make_unique<StateA>()};

    char comm;
    do {
        std::cout << "state: " << fsm.getState() << '\n';
        std::cout << "event: ";
        std::cin >> comm;
        auto event = [&] () -> std::unique_ptr<Event> {
            switch (comm) {
                case 'A':
                    return std::make_unique<EventA>();
                case 'B':
                    return std::make_unique<EventB>();
                default:
                    return std::make_unique<EventA>();
            }
        }();
        std::cout << '\n';
        fsm.dispatch(*event.get());
        /// unique pointer 'event' is destroyed
    } while (comm != 'q');

    return 0;
}
