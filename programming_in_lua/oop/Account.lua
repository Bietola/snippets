Account = {deposit = 0}

function Account:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function Account:withdraw(q)
    local new = self.deposit - q
    if new < 0 then
        error "negative deposit created..."
    else
        self.deposit = new
    end
end

account = Account:new{deposit = 100}
account:withdraw(90)
account:withdraw(11)
account.deposit
