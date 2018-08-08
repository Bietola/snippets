-- turns internal into a read-only table
function readOnly(internal)
	local proxy = {}
	local internal = internal or {}

	local mt = {
		__index = function(tbl, i)
			return internal[i]
		end,
		__newindex = function(tbl, i, v)
			error("trying o access read-only table")
		end
	}
	setmetatable(proxy, mt)

	return proxy
end

-- usage
greeting = readOnly{"hello", "there", "maan"}
greeting[1]        --> "hello"
greeting[1] = "hi" --> error: "trying to access ready-only table"
