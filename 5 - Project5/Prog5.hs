module Prog5 where

reverse' :: [a] -> [a]
reverse' xs = case xs of [] -> []
                         (x:xs) -> reverse' xs ++ [x]

type TimeStamp = (Int,Int,Int) -- finish this type synonym definition

isShorter :: TimeStamp -> TimeStamp -> Int
isShorter (a,b,c) (x,y,z)
   | (a < x)                              = 1
   | (a > x)                              = -1
   | (a == x) && (b < y)                  = 1
   | (a == x) && (b > y)                  = -1
   | (a == x) && (b == y) && (c < z)      = 1
   | (a == x) && (b == y) && (c > z)      = -1
   | (a == x) && (b == y) && (c == z)     = 0

totalSeconds :: TimeStamp -> Int
totalSeconds (x,y,z) = (x * 3600) + (y * 60) + z

isValid :: TimeStamp -> Bool
isValid (x,y,z)
   | (x >= 0) && (y >= 0) && (y < 60) && (z >= 0) && (z < 60)     = True 
   | otherwise                                                    = False 

time2Str :: TimeStamp -> String
time2Str (x,y,z)
   | (x > 9) && (y > 9) && (z > 9) = show x ++ ":" ++ show y ++ ":" ++ show z
   | (x < 9) && (y > 9) && (z > 9) = "0" ++ show x ++ ":" ++ show y ++ ":" ++ show z
   | (x < 9) && (y < 9) && (z > 9) = "0" ++ show x ++ ":0" ++ show y ++ ":" ++ show z
   | (x < 9) && (y > 9) && (z < 9) = "0" ++ show x ++ ":" ++ show y ++ ":0" ++ show z
   | (x < 9) && (y < 9) && (z < 9) = "0" ++ show x ++ ":0" ++ show y ++ ":0" ++ show z
   | (x > 9) && (y < 9) && (z > 9) = show x ++ ":0" ++ show y ++ ":" ++ show z
   | (x > 9) && (y < 9) && (z < 9) = show x ++ ":0" ++ show y ++ ":0" ++ show z
   | (x > 9) && (y > 9) && (z < 9) = show x ++ ":" ++ show y ++ ":0" ++ show z

data Set345 = NonEmptySet [Int]
            | EmptySet
      deriving Show

member :: Int -> Set345 -> Bool
member x (NonEmptySet ys) = 0 < sum [ 1 | y <- ys, x == y ]
member x (EmptySet) = False

size :: Set345 -> Int
size (NonEmptySet ys) = length ys
size (EmptySet) = 0

ins :: Int -> Set345 -> Set345
ins x (EmptySet) = (NonEmptySet [x])
ins x (NonEmptySet ys)
   | member x (NonEmptySet ys)      = NonEmptySet ys
   | otherwise                      = (NonEmptySet (x : ys))


safeLast :: [Int] -> Maybe Int
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeCount :: Int -> [Int] -> Maybe Int
safeCount x [] = Nothing
safeCount x xs
  | 0 == sum [ 1 | y <- xs, x == y]       = Just 0
  | 0 < sum [ 1 | y <- xs, x == y]       = Just (sum [ 1 | y <- xs, x == y])




