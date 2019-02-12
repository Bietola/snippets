#include <iostream>
#include <memory>
#include <string>

template <class Event>
class State {
    public:
        virtual std::unique_ptr<State> onEvent(const Event& e) const = 0;
        virtual std::string toString() const = 0;

};

template <class Event>
std::ostream& operator<<(std::ostream& ostream, const State<Event>& state) {
    ostream << state.toString();
    return ostream;
}

enum class Event {A, B, C, D};

class StateA : public State<Event> {
    public:
        std::unique_ptr<State<Event>> onEvent(const Event& e) const override {
            if (e == Event::A)
                return std::make_unique<StateB>();
        }
        std::string toString() const override { return "StateA"; }
};

class StateB : public State<Event> {
    public:
        std::unique_ptr<State<Event>> onEvent(const Event& e) const override {
            if (e == Event::B)
                return StateA;
        }
        std::string toString() { return "StateB"; } const override;
}

template <Event>
class Fsm {
    protected:
        std::unique_ptr<State<Event>> mState = nullptr;

    public:
        explicit Fsm(std::unique_ptr<State<Event>> state):
            mState(std::move(state)) {}

        // return false on failure
        bool dispatch(const Event& e) {
            if (!mState) return false;

            auto nextState = mState.dispatch(e);
            if (nextState)
                mState = std::move(nextState);
        }
};


int main() {
    Fsm<Event> fsm{std::make_unique<StateA>()};

    fsm.dispatch(Event::A);

    std::cout << fsm.state() << std::endl;

    return 0;
}
