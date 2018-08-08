str = "random thing"

-- find
-- finds the given word (also takes optional offset) and returns encompassing iterators.
i, j = str:find("thing", 0) --> 8 12
str:sub(i, j) --> "thing"

-- match
-- finds the given word and returns it (useful with patterns).
found = str:match("thing") --> "thing"

-- for later use
str = (str .. " "):rep(3)

-- gsub
-- replace occurrences of first with second (can optionally be limited)
str:gsub("thing", "cossa", 1) --> random cossa random thing random thing

-- gmatch
-- return function that iterates over matched pattern
nthings = 0
for mtc in str:gmatch("thing") do
    nthings = nthings + 1
end
nthings --> 3
