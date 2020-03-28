data Exp = Div Exp Exp | Con Integer
  deriving (Show)

type State = Integer

eval :: Exp -> State -> (Integer, State)
eval (Con x) s = (x, 0)
eval (Div lhs rhs) s = let (lres, s') = eval lhs s
                           (rres, s'') = eval rhs s'
                        in (quote lres rres, s'' + 1)
