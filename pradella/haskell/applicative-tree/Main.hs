data Tree a = Leaf a | Branch (Tree a) (Tree a) | Empty
    deriving (Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Branch r l) = Branch (fmap f r) (fmap f l)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Applicative Tree where
    pure = Leaf

    fs <*> xs = concmap (\f -> fmap f xs) fs

concmap f xs = conc $ fmap f xs

conc = foldr app Empty

app Empty t = t
app t Empty = t
app t t' = Branch t t'

main = print $ Branch (Leaf (+1)) (Leaf (*2)) <*> Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) Empty)
