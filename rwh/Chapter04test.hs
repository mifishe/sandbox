-- file: Chapter04test.hs

import Data.List
import Data.Ord
import Data.Maybe
import System.Environment (getArgs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (head:rest) = Just head

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:tail) = case tail of
    [] -> Nothing
    _ -> Just tail

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast list = Just (last list)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (h:tail) = case tail of
    [] -> Nothing
    _  -> Just (init (h:tail))


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith splitCond list = firstHead ++ splitWith splitCond (fromMaybe [] (safeTail rest))
    where

        firstHead = case first of
            []      -> []
            _       -> [first]

        (first, rest) = span splitCond list


firstWordInLine :: String -> String
firstWordInLine [] = ""
firstWordInLine text = word ++ "\n" ++ firstWordInLine restTail
    where
        isLineBreak x = x == '\n'

        (line,rest) = break isLineBreak text

        restTail = case rest  of
            [] -> ""
            _  -> tail rest

        word = case words line of
            []    -> ""
            (w:_) -> w


transpose :: String -> String
transpose [] = ""


interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where
        mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

            -- replace "id" with the name of our function below
        myFunction = firstWordInLine

