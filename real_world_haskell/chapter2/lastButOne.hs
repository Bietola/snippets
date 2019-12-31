lastButOne :: [a] -> Maybe a 
lastButOne (x:(y:[])) = Just x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = Nothing
