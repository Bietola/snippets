primes = {}

primes.cache = {2, 3, 5, 7, 11, 13}

function primes.clearCache()
    primes.cache = {2, 3, 5, 7, 11, 13}
end

function primes.isPrime(num)
    -- using the cache
    for _,prm in ipairs(primes.cache) do
        if num == prm then
            return true
        elseif num % prm == 0 then
            return false
        end
    end
    -- not in cache... naive approach
    local last = primes.cache[#primes.cache]
    for factor=last,math.sqrt(num),2 do
        if num % factor == 0 then
            return false
        end
    end
    return true
end

function primes.next(prev)
    -- increment
    prev = prev + 1
    -- make odd
    if prev % 2 == 0 then
        prev = prev + 1
    end
    -- find next prime
    while true do
        if primes.isPrime(prev) then
            return prev
        end
        prev = prev + 2
    end
end

function primes.iterate(limit)
    local function itr(limit, index)
        -- aliases and defaults
        local cache = primes.cache
        local limit = limit or math.huge

        -- increment
        index = index + 1
        -- over limit
        if index >= limit then
            return nil
        end

        -- number is in cache
        if cache[index] then
            return cache[index]
        -- or not...
        else
            -- calculate number while filling cache
            for _ = #cache,index-1 do
                cache[#cache + 1] = primes.next(cache[#cache])
                print("cache add -> " .. #cache .. ":" .. cache[#cache]) -- DB
            end
            return cache[index]
        end
    end

    return itr, limit, 0
end

itr = primes.iterate()

itr(nil, 5000)
