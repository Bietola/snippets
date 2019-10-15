fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

main = print $ take 5 fib
