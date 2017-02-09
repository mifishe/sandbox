-- file: ch3test.hs

import Data.List
import Data.Ord

myLength :: [t] -> Int
myLength xs = calcLength 0 xs
    where
        calcLength n [] = n
        calcLength n (w:ws) = calcLength (n + 1) ws

myMean [] = 0
myMean xs = (sum xs) / (fromIntegral (length xs))

polindromize [] = []
polindromize (x:xs) = (x : polindromize(xs)) ++ [x]

isPolindrome [] = True
isPolindrome (x:[]) = False
isPolindrome (x:xs)
            | x == (last xs)  = isPolindrome(take (l - 1) xs)
            | otherwise       = False
    where l = length xs

sortByLength [] = [[]]
sortByLength xs = sortBy mCompare xs
    where mCompare a b = compare (length a) (length b)

mIntersperse :: a -> [[a]] -> [a]
mIntersperse sp [] = []
mIntersperse sp (x:[]) = x
mIntersperse sp (x:xs) = x ++ [sp] ++ (mIntersperse sp xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)


treeLength :: Tree a -> Int
treeLength Empty = 0
treeLength (Node x left right)
        | leftLength <= rightLength  = rightLength + 1
        | otherwise      = leftLength + 1
        where leftLength  = treeLength left
              rightLength = treeLength right


data Point = Point {
        x :: Double,
        y :: Double
    } deriving (Show, Eq)


data Direction = Straight
        | Left
        | Right
    deriving (Show)

getDirection :: Point -> Point -> Point -> Direction
getDirection p1 p2 p3
    | a > 0 = Main.Right
    | a < 0 = Main.Left
    | otherwise = Main.Straight
    where a =  ((x p2) - (x p1)) * ((y p3) - (y p1)) - ((y p2) - (y p1))*((x p3) - (x p1))


getDirections :: [Point] -> [Direction]
getDirections (x:y:z:xs) = [(getDirection x y z)] ++ (getDirections (y:[z] ++ xs))
getDirections _ = []


grahamScan :: [Point] -> [Point]
grahamScan points
    | length points >= 3  = scan sortedPoints [p0]
    | otherwise           = points
    where

        p0 = foldl getLowestPoint startPoint points where
                getLowestPoint p1 p2
                    | (y p2) < (y p1) = p2
                    | otherwise       = p1
                (_,maxFloat) = floatRange 0
                startPoint = Point{x=0.0, y = fromIntegral maxFloat}

        polarAngle p1 p2 = atan2 ((y p2) - (y p1)) ((x p2) - (x p1))


        sortedPoints = map
            (\(p,_) -> p)
            (sortBy
                (\(_,a1) (_,a2) -> compare a1 a2)
                (map
                    (\p -> (p, polarAngle p0 p))
                    (filter (/= p0) points)))

        scan (p:pts) (c2:c1:cs) = case dir of
            Main.Right -> scan (p:pts) (c1:cs)
            Main.Left -> scan pts ([p,c2,c1] ++ cs)
            Main.Straight -> scan (pts) ([p,c1] ++ cs)
            where dir = getDirection c2 c1 p


        scan (p:pts) [p0] = scan pts [p, p0]

        --exit
        scan [] ch = ch


fromPoints ((vx,vy):pts) = Point{x=fromIntegral(vx),y=fromIntegral(vy)} : (fromPoints pts)
fromPoints [] = []


testData = fromPoints [(1,2), (1,3), (1,-5), (-1,4), (2,3), (0,2), (-2,1)]
testData2 = fromPoints [(1,-5), (2, 3), (1,2), (1,3), (1,4)]

