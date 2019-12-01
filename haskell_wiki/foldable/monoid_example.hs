-- `Rat`ional number datatype
data Rat = Rat Integer Integer

-- Framework for creating a `Rat`
simplify (Rat n d) = let g = gcd n d in Rat (n `div` g) (d `div` g)
makeRat n d = simplify $ Rat n d
infixr 5 ./ 
(./) = makeRat

-- `Rat` addition
infixr 5 .+
(Rat ln ld) .+ (Rat rn rd) = let l = lcm ld rd in (((ln * (l `div` ld)) + (rn * (l `div` rd)))) ./ l

-- Show `Rat`
instance Show Rat where
    show (Rat n d) = show n ++ "/" ++ show d

-- Monoid with `Rat` addition and 0 / 1
instance Monoid Rat where
    mappend = (.+)
    mempty = 0 ./ 1

main = print $ mconcat ([1, 2, 3] :: [Integer])
