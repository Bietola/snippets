data RPS
  = Rock
  | Paper
  | Scissors
  deriving (Eq)

instance Ord RPS where
  l <= r
    | l == r = True
  Rock <= Paper = True
  Paper <= Scissors = True
  Scissors <= Rock = True
  _ <= _ = False

leftWins l r = l <= r

main = print $ leftWins Rock Rock
