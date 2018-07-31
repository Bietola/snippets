#pragma once

#include <memory>

namespace utl {

// test function
std::string greet(const std::string& toGreet) {
    return "hello " + toGreet + "!!!";
}

// type wrapper for TMP 
template <class T>
class type_wrapper {
    public:
        using type = T;
};

// smart pointer interface for raw pointers 
template <class T, class Deleter = void>
using raw_ptr = T*;

template <class Type, class... Args>
auto make_raw(Args&&... args) {
    return new Type(args...);
}

// CRTP for prototype interface
template <class Base>
class cloneable {
    protected:
        virtual Base* clone_impl() const = 0;
};

template <class Derived, class Base,
          template <class, class> class Ptr = raw_ptr,
          template <class> class Deleter = std::default_delete>
class clone_inherit : public Base {
    public:
        using DerivedPtr = Ptr<Derived, Deleter<Derived>>;
    
    private:
        Base* clone_impl() const { return new Derived(static_cast<const Derived&>(*this)); }

    public:
        DerivedPtr clone() const {
            return DerivedPtr(static_cast<Derived*>(clone_impl()), Deleter<Derived>()); 
        }
};

}
