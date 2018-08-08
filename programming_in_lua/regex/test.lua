-- stringify
function stringify(thing)
    if thing == nil then
        return "nil"
    elseif type(thing) ~= "table" then
        return tostring(thing)
    else
        local result = {}
        for k,v in pairs(thing) do
            result[k] = stringify(v)
        end
        return "[" .. result:concat(", ") .. "]"
    end
end

-- things
str = "(hello) (there) (how) (are)"
things = {}
for match in str:gmatch("%a+") do
    things[#things + 1] = match
end
