#pragma once

#include <memory>

#include <boost/hana.hpp>

namespace hana = boost::hana;

template <class T, class Deleter = void>
using raw_ptr = T*;

template <class Base,
          template <class, class> class _Ptr = raw_ptr,
          template <class> class _Deleter = std::default_delete>
class clonable {
    public:
        template <class T, class D>
        static hana::type<_Ptr<T, D>> sPtrType = hana::type<_Ptr<T, D>>();
        /* struct Ptr { */
        /*     using type = _Ptr<T, D>; */
        /* }; */

        template <class T>
        static hana::type<_Deleter<T>> sDeleterType = hana::type<_Deleter<T>>();
        /* struct Deleter { */
        /*     using type = _Deleter<T>; */
        /* }; */

        using PtrBase = typename decltype(sPtrType<Base, sDeleterType<Base>>)::type;
        using BaseDeleter = typename decltype(sDeleterType<Base>)::type;

    protected:
        virtual Base* clone_impl() const noexcept = 0;

    public:
        PtrBase clone() const noexcept { return PtrBase(clone_impl(), BaseDeleter()); }
};

template <class Base, class Derived>
class clone_inherit : public Base {
    public:
        using PtrDerived = typename
            decltype(
                Base::template sPtrType<
                    Derived,
                    typename
                        decltype(Base::template sDeleterType<Derived>
                    )::type
                >
            )::type;
        using DerivedDeleter = typename decltype(Base::template sDeleterType<Derived>)::type;

    private:
        Base* clone_impl() const noexcept override { return new Derived(); }

    public:
        PtrDerived clone() const noexcept { return PtrDerived(clone_impl(), DerivedDeleter()); }
};
