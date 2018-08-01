#include <iostream>

template <class T>
struct Fancyfier {
    T thing;

    Fancyfier(T t): thing(t) {}
    operator T() const { return thing; }
};

template <class T>
std::ostream& operator<<(std::ostream& ostream, const Fancyfier<T>& fancyfied) {
    ostream << "<~~|" << fancyfied.thing << "|~~>";
    return ostream;
}

template <class T,
          template <class> class Wrap>
class Modifier {
    public:
        using StoredType = T;
        template <class ToWrap> using StoredWrap = Wrap<ToWrap>;

        using WrappedType = Wrap<T>;

        virtual WrappedType modify(const WrappedType&) const = 0;
};

template <class Base>
class OneUpper : public Base {
    public:
        using RetType = typename Base::template StoredWrap<typename Base::StoredType>;

        RetType modify(const RetType& thing) const override { return thing + 1; }
};

class FancyIntModifier : public Modifier<int, Fancyfier> {};

class FancyIntOneUpper : public OneUpper<FancyIntModifier> {};

int main() {
    FancyIntModifier* fiMod = new FancyIntOneUpper;
    std::cout << fiMod->modify(1) << std::endl;

    return 0;
}
