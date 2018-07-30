#pragma once

#include <memory>

namespace utl {

// smart pointer interface for raw pointers 
template <class T, class Deleter = void>
using raw_ptr = T*;

template <class Type, class... Args>
auto make_raw(Args&&... args) {
    return new Type(args...);
}

// CRTP interface for implmenting the protorype pattern
template <class BaseClass,
          template <class T, class Deleter> class PtrType = raw_ptr,
          class Deleter = std::default_delete<BaseClass>>
class clonable {
    public:
        using BasePtr = PtrType<BaseClass, Deleter>;

        virtual BasePtr clone() const noexcept = 0;

        template <class ChildClass, class... Args>
        static BasePtr make(Args&&... args) noexcept { 
            return BasePtr(new ChildClass(std::forward<Args>(args)...), Deleter());
        }
};

template <class BaseClass, class ChildClass,
          template <class T, class Deleter> class PtrType = raw_ptr,
          class Deleter = std::default_delete<ChildClass>>
class clonable_impl: public BaseClass {
    public:
        using ChildPtr = PtrType<ChildClass, Deleter>;

        ChildPtr clone() const noexcept override {
            return Ptr(new ChildClass(*this), Deleter());
        }
};

};
