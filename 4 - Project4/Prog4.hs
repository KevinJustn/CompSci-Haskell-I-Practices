module Prog4 where
import Data.Char

tripleAll :: [Int] -> [(Int, Int)]
tripleAll [] = []
tripleAll (x:xs) = (x,x*3) : tripleAll xs

flip' :: [(Int, Int)] -> [(Int, Int)]
flip' [] = []
flip' ((x,y):xs) = (y,x) : flip' xs

sumLastPart :: Int -> [Int] -> Int
sumLastPart _ []  = 0
sumLastPart x y 
  | x < (length y)    = sumLastPart x (drop 1 y)
  | x == (length y)   = sum' y  

----- helper ----- 
sum' :: [Int] -> Int 
sum' [] = 0
sum' (x:xs) = x + sum' xs
------------------

middleProduct :: [Int] -> Int
middleProduct [] = 0
middleProduct xs = prod'(init(tail xs))

----- helper ----- 
prod' :: [Int] -> Int 
prod' [] = 1
prod' (x:xs) = x * prod' xs
------------------

init' :: [Int] -> [Int]
init' (x:xs) 
  | length xs == 0                  = []
  | x == (xs !! (length xs - 1))    = []
  | x /= (xs !! (length xs - 1))    = x : init' xs

lowerOddLetters :: String -> String
lowerOddLetters [] = []
lowerOddLetters xs = helplOL xs [0..(length xs)]

---- helper ----- 
helplOL :: String -> [Int] -> String
helplOL xs (y:ys) 
  | ys == []    = []
  | even y && ((xs !! y) >= 'A' && (xs !! y) <= 'Z')  = [toLower (xs !! y)] ++ helplOL xs ys
  | otherwise       = [xs !! y] ++ helplOL xs ys
-----------------

elemAt :: Int -> [Int] -> Int
elemAt x xs
  | x == length xs  = xs !! (x-1)
  | otherwise       = elemAt x (init xs)



iSort' :: [(String, Int)] -> [(String, Int)]
iSort' []   = []
iSort' (x:xs) = helpiS (x) (iSort' xs)

---- helper -----
helpiS :: (String,Int) -> [(String,Int)] -> [(String, Int)]
helpiS x [] = [x]
helpiS x (y:ys)
  | (snd x) < (snd y)   = x:y:ys
  | otherwise = y : (helpiS x ys)
-----------------}


middleWord :: String -> String
middleWord stg = reverse (drop (1 + helpmW (reverse (stg)) 0) (reverse (drop (1 + helpmW stg 0) stg)))

---- helper -----
helpmW :: String -> Int -> Int
helpmW str x 
  | x > (length str)    = 0
  | (str !! x) == ' '   = x
  | otherwise           = helpmW str (x+1)
-----------------

lowerFirstLetter :: String -> String
lowerFirstLetter (x:xs)
  | xs == []              = [x]
  | x >= 'A' && x <= 'Z'  = [toLower x] ++ xs
  | otherwise             = [x] ++ lowerFirstLetter xs





