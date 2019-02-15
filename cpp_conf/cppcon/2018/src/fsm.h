#pragma once

#include <memory>

#include "State.h"
#include "Event.h"

class Fsm {
    protected:
        std::unique_ptr<State> mState = nullptr;

    public:
        explicit Fsm(std::unique_ptr<State> state):
            mState(std::move(state)) {}

        // getters
        const State& getState() const { return *mState.get(); }

        // return false on failure
        bool dispatch(const Event& e) {
            if (!mState) return false;

            auto nextState = e.onState(*mState);
            if (nextState)
                mState = std::move(nextState);
            return true;
        }
};
