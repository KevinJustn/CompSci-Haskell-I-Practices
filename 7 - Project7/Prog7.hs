module Prog7 where
import Data.Char

piglatinize :: String -> String
piglatinize (x:xs) 
   | isVowel x    = x : xs ++ "yay"
   | otherwise    = piglatinize2 (xs ++ [x])
piglatinize _ = " "

piglatinize2 :: String -> String
piglatinize2 (x:xs) 
   | isVowel x    = x : xs ++ "ay"
   | otherwise    = piglatinize2 (xs ++ [x])
piglatinize2 _ = " "

isVowel :: Char -> Bool 
isVowel ch = (ch == 'a') || (ch == 'e') || (ch == 'i') || (ch == 'o') || (ch == 'u')

data Expr1 = Val1 Int
            | Add1 Expr1 Expr1
            | Sub1 Expr1 Expr1
            | Mul1 Expr1 Expr1
            | Div1 Expr1 Expr1

instance Show Expr1 where
   show (Val1 x)     = show x
   show (Add1 x y)   = "(" ++ show x ++ "+" ++ show y ++")"
   show (Sub1 x y)   = "(" ++ show x ++ "-" ++ show y ++")"
   show (Mul1 x y)   = "(" ++ show x ++ "*" ++ show y ++")"
   show (Div1 x y)   = "(" ++ show x ++ "/" ++ show y ++")"

value1 :: Expr1 -> Int
value1 (Val1 x) = x
value1 (Add1 x y) = (value1 x) + (value1 y)
value1 (Sub1 x y) = (value1 x) - (value1 y)
value1 (Mul1 x y) = (value1 x) * (value1 y)
value1 (Div1 x y) = quot (value1 x) (value1 y)



value2 :: Expr1 -> Maybe Int
value2 (Val1 x) = Just (x)
value2 (Add1 x y) = Just ((value1 x) + (value1 y))
value2 (Sub1 x y) = Just ((value1 x) - (value1 y))
value2 (Mul1 x y) = Just ((value1 x) * (value1 y))
value2 (Div1 x y) 
   | (value1 y) == 0      = Nothing
   | otherwise          = Just (quot (value1 x) (value1 y))

sumSqNeg :: [Int] -> Int
sumSqNeg xs = sum(map sqNum (filter negative xs))

sqNum :: Int -> Int 
sqNum x = x * x

negative :: Int -> Bool
negative x = x < 0

containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing [] _ = True
containing _ [] = False
containing (x:xs) ys 
   | elem x ys       = containing xs ys 
   | otherwise       = False

containing2 :: Eq a => [a] -> [a] -> [Bool]
containing2 [] [] = [True]
containing2 [] _ = [True]
containing2 _ [] = [False]
containing2 (x:xs) ys 
   | elem x ys       = [True] ++ containing2 xs ys 
   | otherwise       = [False]

containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = all isTrue (containing2 xs ys)

isTrue :: Bool -> Bool
isTrue x = x

total :: (Int -> Int) -> [Int] -> Int
total f xs = sum ([f x | x <- xs])