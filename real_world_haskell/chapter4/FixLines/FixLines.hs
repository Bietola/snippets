import System.Environment (getArgs)
import Data.List

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction''
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments are needed"

        myFunction = unlines . map fixLine . lines
          where fixLine x | last x == '\r' = init x
                          | otherwise = x
