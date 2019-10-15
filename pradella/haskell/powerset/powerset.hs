powerset :: Show a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x:ss | ss <- powerset xs]

main :: IO ()
main = print $ powerset [1, 2, 3]
