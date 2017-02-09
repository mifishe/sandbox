-- file: ch04/getFirstWords.hs
-- Save this in a source file, e.g., Interact.hs

import System.Environment (getArgs)

firstWordInLine :: String -> String
firstWordInLine [] = ""
firstWordInLine text = word ++ "\n" ++ firstWordInLine tailText
    where
        isLineBreak x = x == '\n'

        (line,rest) = break isLineBreak text

        tailText = case rest  of
            [] -> ""
            _  -> tail rest

        word = case words line of
            []    -> ""
            (w:_) -> w

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = firstWordInLine
