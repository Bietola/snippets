-- Hello world
print "hello world!"

-- Lists are Applicative Functors
[(* 1), (+ 1)] <*> [1, 2, 3] -- > [1, 2, 3, 5, 6, 7]

-- IO monad example 
main :: IO String
main = do
    x <- return "hello"
    return x
