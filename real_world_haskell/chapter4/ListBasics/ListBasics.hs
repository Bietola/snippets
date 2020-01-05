take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take (n - 1) xs

integers = go 0
  where go n = n : go (n + 1)

drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

drop'' n = fst . foldl step ([], n)
  where step s@(_, 0) _ = s
        step (s, n) e   = (e:s, n - 1)
