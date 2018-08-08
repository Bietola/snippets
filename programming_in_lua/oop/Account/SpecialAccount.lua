SpecialAccount = Account:new{limit = 100}

function SpecialAccount:withdraw(q)
    if q > self.limit then
        error "special withdrawl over limit..."
    else
        self.deposit = self.deposit - q
    end
end
