data MyInt = MyInt Integer
  deriving(Show)

data MyAdder = MyAdder (MyInt -> MyInt)

instance Monad MyAdder where
  return = MyAdder

instance Semigroup MyAdder where
  MyAdder lhs <> MyAdder rhs = MyAdder (lhs . rhs)

instance Monoid MyAdder where
  mappend = (<>)
  mempty = MyAdder (return (mappend MyInt))
