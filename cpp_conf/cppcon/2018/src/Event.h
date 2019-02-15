#pragma once

#include <memory>
#include <string>

#include "State.h"

// Base class.
class Event {
    public:
        virtual std::unique_ptr<State> onState(const State& state) const = 0;

        virtual ~Event() {}
};

// CRTP for double dispatch.
template <class Derived>
class EventBase : public Event {
    public:
        std::unique_ptr<State> onState(const State& state) const override {
            return state.onEvent(*static_cast<const Derived*>(this));
        }

        virtual ~EventBase() {}
};

// EventA.
class EventA : public EventBase<EventA> {
    public:
        EventA() = default;
        EventA(const std::string& str):
            mMsg(str) {}

        ~EventA() {}

    private:
        std::string mMsg;
};

// EventB.
class EventB : public EventBase<EventB> {
    public:
        EventB() = default;

        ~EventB() {}
};
