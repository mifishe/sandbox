-- file: ch04\ch04exercises02.hs

import Data.Char (digitToInt)

loop :: Int -> String -> Int
loop acc [] = acc

loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs

asInt_foldl :: String -> Int
asInt_foldl ('-':xs) = -1 * (asInt_foldl xs)
asInt_foldl xs = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x

asInt_foldr :: String -> Int
asInt_foldr ('-':xs) = -1 * (asInt_foldr xs)
asInt_foldr xs = result
    where
        (result, _) = foldr step (0, 1) xs
        step x (acc, m)  = (acc + m * (digitToInt x), m * 10)
