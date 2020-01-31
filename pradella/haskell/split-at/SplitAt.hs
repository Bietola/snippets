splitAt' = go []
  where go acc 0 rest   = (acc, rest)
        go acc n (x:xs) = go (acc ++ [x]) (n - 1) xs
