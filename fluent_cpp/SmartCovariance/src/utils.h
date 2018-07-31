#pragma once

#include <memory>

namespace utl {

// test function
std::string greet(const std::string& toGreet) {
    return "hello " + toGreet + "!!!";
}

// smart pointer interface for raw pointers 
template <class T, class Deleter = void>
using raw_ptr = T*;

template <class Type, class... Args>
auto make_raw(Args&&... args) {
    return new Type(args...);
}

// CRTP for prototype interface
template <class Base,
          template <class, class> class _Ptr = raw_ptr,
          template <class> class _Deleter = std::default_delete>
class cloneable {
    public:
        template <class T1, class T2>
        using Ptr = _Ptr<T1, T2>;

        template <class T>
        using Deleter = _Deleter<T>;

        using BasePtr = Ptr<Base, Deleter<Base>>;

        BasePtr clone() const { return BasePtr(clone_impl(), Deleter<Base>()); }

    protected:
        virtual Base* clone_impl() const = 0;
};

template <class Derived, class Base>
class clone_inherit : public Base {
    public:
        template <class T1, class T2> 
        using Ptr = class Base::Ptr<T1, T2>;

        template <class T>
        using Deleter = class Base::Deleter<T>;

        using DerivedPtr = Ptr<Derived, Deleter<Derived>>;
    
    private:
        Base* clone_impl() const { return new Derived(static_cast<const Derived&>(*this)); }

    public:
        DerivedPtr clone() const {
            return DerivedPtr(static_cast<Derived*>(clone_impl()), Deleter<Derived>()); 
        }
};

}
