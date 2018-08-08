require("Account")

SpecialAccount = Account:new{limit = 100}

function SpecialAccount:withdraw(q)
    if q > limit then
        error "trying to withdraw too much"
    else
        self.deposit = self.deposit - q
    end
end
