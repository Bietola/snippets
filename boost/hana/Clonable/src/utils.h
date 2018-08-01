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
        template <class T, template <class> class D>
        using Ptr = type_wrapper<_Ptr<T, D<T>>>;

        template <class T>
        using Deleter = type_wrapper<_Deleter<T>>;

        using BasePtr     = _Ptr<Base, _Deleter<Base>>;
        using BaseDeleter = _Deleter<Base>;

    protected:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        BasePtr clone() const noexcept { return BasePtr(clone_impl(), BaseDeleter()); }
};

template <class Base, class Derived>
class clone_inherit : public Base {
    public:
        template <class T, class D>
        using _Ptr = class Base::template Ptr<T, D>::type;

        template <class T>
        using _Deleter = class Base::template Deleter<T>::type;

        using DerivedPtr     = _Ptr<Derived, _Deleter<Derived>>;
        using DerivedDeleter = _Deleter<Derived>;

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
