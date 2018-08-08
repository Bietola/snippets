Account = {deposit = 0}

function Account:new(obj)
    obj = obj or {}
    self.__index = self
    setmetatable(obj, self)
    return obj
end

function Account:withdraw(q)
    local new = self.deposit - q
    if new < 0 then
        error "negative deposit..."
    else
        self.deposit = new
    end
end
