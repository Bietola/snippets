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
        template <class... Args> 
        using Ptr = _Ptr<Args...>;

        template <class... Args>
        using BasePtr = Ptr<Base, Args...>;

    private:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        template <class Derived, class... PtrArgs, class... Args>
        static Ptr<Derived, PtrArgs...> make_ptr(Args&&... args) {
            return Ptr<Derived, PtrArgs...>(
                new Derived(std::forward(args)...),
                PtrArgs()...
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
