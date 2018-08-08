-- table to access gTbl (always empty)
function track(toTrack)
	local toTrack = toTrack or {}
	local accTbl = {} 
	local mt = {
		__index = function(tbl, i)
			print("getting " .. tostring(i))
			return toTrack[i]
		end,
		__newindex = function(tbl, i, nval)
			print("setting " .. tostring(i) .. " to " .. tostring(nval))
			toTrack[i] = nval
		end
	}
	setmetatable(accTbl, mt)
	return accTbl
end

-- example
tbl = {"ibra", "timon", "pumba"}
tbl = track(tbl)
print(tbl[2]) --> getting 1
