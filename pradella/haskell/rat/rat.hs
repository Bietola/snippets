data Rat =
  Rat !Integer !Integer
  deriving (Eq)

makeRat n d = simplify $ Rat n d

simplify (Rat n d) =
  let gcd' = gcd n d
   in Rat (n `div` gcd') (d `div` gcd')

instance Show Rat where
  show (Rat n d) = show n ++ "/" ++ show d

instance Num Rat where
  (Rat nl dl) + (Rat nr dr) =
    let lcm' = lcm dl dr
     in simplify (Rat (nl * (lcm' `div` dl) + nr * (lcm' `div` dr)) lcm')
  (Rat nl dl) - (Rat nr dr) =
    let lcm' = lcm dl dr
     in simplify (Rat (nl * (lcm' `div` dl) - nr * (lcm' `div` dr)) lcm')
  (Rat nl dl) * (Rat nr dr) = makeRat (nl * nr) (dl * dr)
  signum (Rat n d) = makeRat (signum n * signum d) 1
  fromInteger n = makeRat n 1
  abs rat = rat * signum rat

instance Ord Rat where
  (Rat nl dl) <= (Rat nr dr) = nl * dr <= nr * dl

main =
  print $
  let x = -(fromInteger 2 + fromInteger 1)
   in abs x == x
