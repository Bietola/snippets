#pragma once

#include <iostream>
#include <string>
#include <memory>

// Forward declarations.
class Event;
class EventA;
class EventB;

// Base class.
class State {
    public:
        virtual std::unique_ptr<State> onEvent(const EventA&) const { return nullptr; }
        virtual std::unique_ptr<State> onEvent(const EventB&) const { return nullptr; }
        virtual std::string toString() const = 0;

        virtual ~State() {};
};
// Base class friend functions.
inline std::ostream& operator<<(std::ostream& ostream, const State& state) {
    ostream << state.toString();
    return ostream;
}

// StateA class.
class StateA : public State {
    public:
        std::unique_ptr<State> onEvent(const EventA&) const override;
        std::unique_ptr<State> onEvent(const EventB&) const override;
        std::string toString() const override { return "StateA"; }
};

// StateB class.
class StateB : public State {
    public:
        std::unique_ptr<State> onEvent(const EventA&) const override;
        std::unique_ptr<State> onEvent(const EventB&) const override;
        std::string toString() const override { return "StateB"; }
};
