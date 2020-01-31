module RecMon where

data CashRegister a = CashRegister { getReceipt :: (a, Float) } deriving (Show, Eq)

getCurrentItem = fst . getReceipt

getPrice = snd . getReceipt

instance Functor CashRegister where
  fmap f (CashRegister (item, cost)) = CashRegister (f item, cost)

instance Applicative CashRegister where
  pure itm = CashRegister (itm, 1.0)

  CashRegister (f, _) <*> CashRegister (x, _) = CashRegister (f x, 1.0)

instance Monad CashRegister where
  CashRegister (itm, cst) >>= f = f itm

test :: CashRegister String
test = pure ("red "++) <*> pure "apple"

test1 :: CashRegister String
test1 = pure (("red "++) "apple")

test2 :: CashRegister String
test2 = pure (("red "++) "apple")
