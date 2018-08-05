function find_if(list, cond)
	if list == nil then
		return nil
	elseif cond(list.head) then
		return list.head
	else
		return find_if(list.tail, cond)
	end
end

function reverse(arr)
	local n = #arr
	local rev = {}
	for i,v in ipairs(arr) do
		rev[n - i + 1] = v 
	end
	return rev
end

function make_list(...) 
	local list = nil
	for _,v in ipairs(reverse{...}) do
		list = {tail = list, head = v}
	end
	return list
end

myList = make_list(1, 2, 3, 4, 5)

foundIt = find_if(make_list(1, 1, 1, 1, 2, 3, 4),
	function(ele)
		return ele % 2 == 0
	end
)
print(foundIt)
