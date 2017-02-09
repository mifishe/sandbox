-- file: ch04/transposeFile.hs
-- Save this in a source file, e.g., Interact.hs

import System.Environment (getArgs)

mTranspose :: String -> String
mTranspose [] = ""
mTranspose text = transposeLines (lines text)
    where
        transposeLines [] = []
        transposeLines textLines =
            case newStr of
                [] -> ""
                _  -> newStr ++ "\n" ++ transposeLines (map lineTail splittedLines)
            where
                newStr = buildUpLine splittedLines;
                buildUpLine [] = ""
                buildUpLine (p:ps) = ch ++ buildUpLine ps
                    where
                        (ch, _) = p

                splittedLines = map splitChar textLines
                splitChar = splitAt 1
                lineTail (c,l) = l


interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = mTranspose
