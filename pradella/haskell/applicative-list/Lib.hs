module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f Nil = Nil

append xs Nil = xs
append (Cons x Nil) xs = Cons x xs
append (Cons x xs) xs' = Cons x $ append xs xs'

conc (Cons (Cons xx xxs) xs) = Cons xx xxs `append` conc xs
conc (Cons Nil xs) = conc xs
conc Nil = Nil

concMap f xs = conc $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = concMap (\f -> fmap f xs) fs

main = print $ (Cons (+ 1) (Cons (+ 2) Nil)) <*> (Cons 1 (Cons 2 Nil))
