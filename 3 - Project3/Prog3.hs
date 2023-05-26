module Prog3 where
import Data.Char

tripleAll :: [Int] -> [(Int, Int)]
tripleAll xs = [ (x,x*3) | x <- xs]

flip' :: [(Int, Int)] -> [(Int, Int)]
flip' xs = [ (y, x) | (x,y) <- xs]

sumLastPart :: Int -> [Int] -> Int
sumLastPart x xs = sum (drop ((length xs) - x) xs)

middleProduct :: [Int] -> Int
middleProduct xs = product (tail (reverse (tail xs)))

init' :: [Int] -> [Int]
init' xs = reverse (tail (reverse xs))

triads :: Int -> [(Int,Int,Int)]
triads q = [ (x,y,helptri x y) | x <- [0..q], y <- [x..q], helptri x y <= q] -- might not work

helptri :: Int -> Int -> Int 
helptri x y = ceiling(sqrt(fromIntegral(x^2 + y^2)))

pushRight :: String -> Int -> String
pushRight str x = replicate (x - length(str)) ' ' ++ str

lowerFirstCharacter :: String -> String
lowerFirstCharacter str = [toLower(head str)] ++ tail str

elemAt :: Int -> [Int] -> Int
elemAt x xs = head((drop (x - 1) xs))

middleWord :: String -> String
middleWord xs = drop (1 + helpmiddleWord xs !! 0) (take (helpmiddleWord xs !! 1) xs)

helpmiddleWord :: String -> [Int]
helpmiddleWord str = [ x | x <- [0..length str], (str !! x) == ' ']
