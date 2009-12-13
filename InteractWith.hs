import Data.List
import System.Environment (getArgs)

firstOfEachLine text = unwords firstWords
  where firstWord line = head (words line)
        firstWords = firstWord `map` (lines text)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = firstOfEachLine
        
transposeText text = unlines (transpose (lines "hello\nworld\n"))