-- file: ch04\ch04exercises02.hs

import Data.Char (digitToInt, isDigit)

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


type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = case asInt_either xs of
    Left msg  -> Left msg
    Right val -> Right (negate val)
asInt_either xs = foldl step (Right 0) xs
    where
        step acc x = case acc of
            Left msg  -> acc
            Right val -> case (isDigit x) of
                False -> Left ("non-digit '" ++ [x] ++ "'")
                otherwise -> Right (val * 10 +  digitToInt x)


concat_foldr :: [[a]] -> [a]
concat_foldr [] = []
concat_foldr xs = foldr step [] xs
    where
        step [] acc = acc
        step x acc = x ++ acc

takeWhile_rec :: (a -> Bool) -> [a] -> [a]
takeWhile_rec _ [] = []
takeWhile_rec pred (x:xs) = case (pred x) of
    True -> x : takeWhile_rec pred xs
    False -> []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr _ [] = []
takeWhile_foldr pred xs = foldr step [] xs
    where
        step x acc = case (pred x) of
                False -> []
                True  -> x : acc

groupBy_foldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldr _ [] = []
groupBy_foldr validator xs = foldr step [] xs
    where
        step x [] = [[x]]
        step x ((y:ys):prev) = case validator x y of
            True -> (x:(y:ys)) : prev
            False -> [x] : ((y:ys):prev)



