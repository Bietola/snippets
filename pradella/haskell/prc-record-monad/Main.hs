module RecMon where

data CashRegister a = CashRegister { getReceipt :: (a, Float) } deriving (Show, Eq)

getCurrentItem = fst . getReceipt

getPrice = snd . getReceipt

instance Functor CashRegister where
  fmap f (CashRegister (item, cost)) = CashRegister (f item, cost)

instance Applicative CashRegister where
  pure itm = CashRegister (itm, 0.0)

  CashRegister (f, p1) <*> CashRegister (x, p2) = CashRegister (f x, p1 + p2)

instance Monad CashRegister where
  CashRegister (itm, cst) >>= f = let fres = f itm
                                   in CashRegister (getCurrentItem fres, getPrice fres + cst)

test :: CashRegister String
test = pure ("red "++) <*> pure "apple"

test1 :: CashRegister String
test1 = pure (("red "++) "apple")

test2 :: CashRegister String
test2 = pure (("red "++) "apple")
