module Lib where

import Text.Regex.Posix

data Unit = Knots | MPS

-- e.g.: 11023KT -> 23; 32321MPS -> 21 
windSpeed :: String -> (Int, Unit)
windSpeed str =
  case matchRegex "[0-9]{3}([0-9]{2,3})(KT|MPS)" of
    res@(_, _) -> res
    _ -> error "The fuck you say to me?"