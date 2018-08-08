-- stringify
function stringify(thing)
    if thing == nil then
        return "nil"
    elseif type(thing) ~= "table" then
        return tostring(thing)
    else
        local result = {}
		local i = 1
        for k,v in pairs(thing) do
            result[i] = tostring(k) .. ":" .. stringify(v)
			i = i + 1
        end
        return "[" .. table.concat(result, ", ") .. "]"
    end
end
