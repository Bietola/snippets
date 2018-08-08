
-- /bigtag{bigtxt /tag{text}} -> <bigtag>bigtext <tag>text</tag></bigtag>
function naive-latex2xml(latex)
	local result, count = latex 
	repeat
		result, count = result:gsub("/([^{}]+){([^{}]-)}", "<%1>%2</%1>")
	until count == 0
	return result
end

function latex2xml(latex)
	return (latex:gsub("/(%a.-){(.*)}", function(tag, body)
		return "<" .. tag .. ">" .. latex2xml(body) .. "</" .. tag .. ">"
	end))
end

latex2xml("/bigtag{bigtxt /tag{text}}")
