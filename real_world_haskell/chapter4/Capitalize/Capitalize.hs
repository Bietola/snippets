import System.Environment (getArgs)
import Data.Char (toUpper)

interactWith function inputFile output = do
  input <- readFile inputFile
  writeFile output (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments are needed"

        myFunction = unwords . map capitalize . words

        capitalize w = (toUpper . head) w : tail w
