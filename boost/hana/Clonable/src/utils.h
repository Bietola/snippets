#pragma once

#include <memory>

namespace utl {

template <class T>
struct type_wrapper {
    using type = T;
};

template <class Base,
          template <class, class> class _Ptr,
          template <class> class _Deleter>
class clonable {
    public:
        template <class T, class D>
        using Ptr = _Ptr<T, D>;
        template <class T>
        using Deleter = _Deleter<T>;

        using BasePtr     = Ptr<Base, Deleter<Base>>;
        using BaseDeleter = Deleter<Base>;

    protected:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        BasePtr clone() const noexcept { return BasePtr(clone_impl(), BaseDeleter()); }
};

template <class Base, class Derived>
class clone_inherit : public Base {
    public:
        template <class T, class D>
        using Ptr = Base::Ptr<T, D>;
        template <class T>
        using Deleter = Base::Deleter<T>;

        using DerivedPtr     = Ptr<Derived, Deleter<Derived>>;
        using DerivedDeleter = Deleter<Derived>;

    private:
        Base* clone_impl() const noexcept override { 
            return new Derived(static_cast<const Derived&>(*this)); 
        }

    public:
        DerivedPtr clone() const noexcept {
            return DerivedPtr(static_cast<Derived*>(clone_impl()), DerivedDeleter());
        }
};

}
