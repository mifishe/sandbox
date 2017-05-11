-- file: ch04\ch04exercises01.hs

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

