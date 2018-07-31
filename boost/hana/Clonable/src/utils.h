#pragma once

#include <memory>

namespace utl {

template <class Base>
class clonable {
    protected:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        std::unique_ptr<Base> clone() const noexcept { return std::unique_ptr<Base>(clone_impl()); }
};

template <class Base, class Derived>
class clone_inherit : public Base {
    private:
        Base* clone_impl() const noexcept override { 
            return new Derived(static_cast<const Derived&>(*this)); 
        }

    public:
        std::unique_ptr<Derived> clone() const noexcept {
            return std::unique_ptr<Derived>(static_cast<Derived*>(clone_impl()));
        }
};

}
