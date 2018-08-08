require("Account")
require("SpecialAccount")

account = SpecialAccount:new{
    deposit = 100,
    limit = 10,
}
account:withdraw(5)
account:withdraw(5)
account:withdraw(10)
account:withdraw(10)
account:withdraw(11)
account.deposit
