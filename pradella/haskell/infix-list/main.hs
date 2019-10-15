infixr 5 ::: 
data List a = Nil | a ::: (List a)
    deriving (Show)

main = print $ 1 ::: 2 ::: 3 ::: Nil
