#pragma once

#include <memory>

namespace utl {

template <class T>
using raw_ptr = T*;

template <class Base,
          template <class...> class _Ptr = raw_ptr,
          template <class> class _Deleter = std::default_delete>
class clonable {
    public:
        template <class T, class D> 
        using Ptr = _Ptr<T, D>;
        template <class T>
        using Deleter = _Deleter<T>;

        using BaseDeleter = Deleter<Base>;
        using BasePtr     = Ptr<Base, BaseDeleter>;

    private:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        template <class Derived, class... Args>
        static Ptr<Derived, Deleter<Derived>> make_ptr(Args&&... args) {
            return Ptr<Derived, Deleter<Derived>>(
                new Derived(std::forward(args)...),
                Deleter<Derived>()
            );
        }

        BasePtr clone() const noexcept { return BasePtr(clone_impl(), BaseDeleter()); }
};

template <class Base, class Derived>
class clone_inherit : public Base {
    public:
        using DerivedDeleter = typename Base::template Deleter<Derived>;
        using DerivedPtr     = typename Base::template Ptr<Derived, DerivedDeleter>;

    private:
        virtual clone_inherit* clone_impl() const noexcept {
            return new Derived(*static_cast<const Derived*>(this));
        }

    public:
        DerivedPtr clone() const noexcept {
            return DerivedPtr(static_cast<const Derived*>(clone_impl()), DerivedDeleter());
        }
};

}
