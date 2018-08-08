-- special characters =, &, +
-- sp.char. -> %xx
-- spaces   -> +
-- key = "value"; key2 = "value2" ->
--     -> key=value&key2=value2
function decodeHtml(htmlText)
	local pattern = "(%a[%w%%]*)=([%w%%]+)"
	local result = {}
	local lastPos
	-- print("setting iterator")
	-- itr = htmlText:gmatch(pattern .. "&()")
	for k,v,lastPos in htmlText:gmatch(pattern .. "&()") do
		print(k .. "; " .. v .. "; " .. lastPos)
		result[k] = v
	end
	local k,v = htmlText:sub(lastPos):match(pattern)
	result[k] = v
end

str = "a random string"
itr = str:gmatch("random()")
str:sub(1, itr())

decodeHtml("x=1&y=2")
