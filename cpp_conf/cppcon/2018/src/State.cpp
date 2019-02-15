#include "State.h"

// StateA class.
std::unique_ptr<State> StateA::onEvent(const EventA&) const {
    return std::make_unique<StateA>();
}
std::unique_ptr<State> StateA::onEvent(const EventB&) const {
    return std::make_unique<StateB>();
}

// StateB class.
std::unique_ptr<State> StateB::onEvent(const EventA&) const {
    return std::make_unique<StateA>();
}
std::unique_ptr<State> StateB::onEvent(const EventB&) const {
    return std::make_unique<StateB>();
}
