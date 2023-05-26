-- In this program, functions can be run individually by using ghci and typing the 
-- function name and the appropriate inputs. 
-- For example, the function "isTwoDigitPositive" requires one input, and outputs a Boolean
-- whereas "dividesEvenly" requires two inputs and outputs a boolean.

module Prog1 where


isTwoDigitPositive :: Integer -> Bool
isTwoDigitPositive x = x >= 9


dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly x y = mod x y == 0


middle :: Integer -> Integer -> Integer -> Integer
middle x y z
  | (x >= y && x <= z) || (x <= y && x >= z)   = x
  | (y >= x && y <= z) || (y <= x && y >= z)   = y 
  | (z >= x && z <= y) || (z >= x && z <= y)   = z


nand :: Bool -> Bool -> Bool
nand x y
  | (x == True && y == True) = False
  | otherwise                = True


f2c :: Float -> Float
f2c x = (x-32)*(5/9)


floorDecimal :: Float -> Float
floorDecimal x = fromIntegral (floor x)


letterGrade :: Integer -> String
letterGrade x
  | (x >= 93)             = "A"
  | (x >= 90 && x <= 92)  = "A-"
  | (x >= 87 && x <= 89)  = "B+"
  | (x >= 83 && x <= 86)  = "B"
  | (x >= 80 && x <= 82)  = "B-"
  | (x >= 77 && x <= 79)  = "C+"
  | (x >= 73 && x <= 76)  = "C"
  | (x >= 70 && x <= 72)  = "C-"
  | (x >= 67 && x <= 69)  = "D+"
  | (x >= 63 && x <= 66)  = "D"
  | (x >= 60 && x <= 62)  = "D-"
  | (x >= 0 && x <= 59)   = "F"
   


averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x+y+z)/3


howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage x y z
  | fromInteger x < fromIntegral (x+y+z)/3 && fromInteger y < fromIntegral (x+y+z)/3 = 2
  | fromInteger x < fromIntegral (x+y+z)/3 && fromInteger z < fromIntegral (x+y+z)/3 = 2
  | fromInteger y < fromIntegral (x+y+z)/3 && fromInteger z < fromIntegral (x+y+z)/3 = 2
  | fromInteger x < fromIntegral (x+y+z)/3 = 1
  | fromInteger y < fromIntegral (x+y+z)/3 = 1 
  | fromInteger z < fromIntegral (x+y+z)/3 = 1
  | otherwise = 0
