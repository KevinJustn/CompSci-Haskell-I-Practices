module Prog2 where

twoSame :: Integer -> Integer -> Integer -> Bool
twoSame x y z
  | x == y || x == z    = True 
  | y == z              = True 
  | otherwise           = False

sum' :: Integer -> Integer
sum' 0      = 0
sum' n     = n + sum'(n-1)

abssum' :: Integer -> Integer
abssum' 0     = 0
abssum' n     = n + sum'(n-1) - (n + sum'(n-1))

or' :: Bool -> Bool -> Bool
or' False False   = False 
or' _  _           = True

integerSqrt :: Integer -> Integer
integerSqrt n = floor(sqrt(fromIntegral(n)))

exponent' :: Integer -> Integer -> Integer
exponent' x y
  | y == 0     = 1
  | y == 1     = x
  | otherwise  = x * exponent'(x) (y-1)

swap :: (Char, Char, Char, Char, Char) -> (Char, Char, Char, Char, Char)
swap (a, b, c, d, e) = (e, d, c, b, a)

negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits td = [ helpTwoDig y | y <- td]

helpTwoDig :: Integer -> Integer
helpTwoDig x
  | x > 9 && x < 99      = -x
  | x < -9 && x > -99    = -x
  | otherwise            = x

matches :: Integer -> [Integer] -> [Integer]
matches mch lst = [ mch | x <- lst, x == mch]

element :: Integer -> [Integer] -> Bool
element mch lst = [] /= matches mch lst
