-- Naive version
powerset :: Show a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x:ss | ss <- powerset xs]

-- Tail recursive version
tailPowerset lst = powerset lst [[]] where
    powerset [] out = out
    powerset (x:xs) out = powerset xs (out ++ [x:ss | ss <- out])

main :: IO ()
main = do
    print "naive: "
    print $ powerset [1, 2, 3]
    print "with tail recursion: "
    print $ tailPowerset [1, 2, 3]
