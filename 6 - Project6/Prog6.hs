module Prog6 where

data Set345 = NonEmptySet [Int]
            | EmptySet
      deriving Show

singletonOrEmpty :: Set345 -> Bool
singletonOrEmpty EmptySet = True
singletonOrEmpty (NonEmptySet ys) = 1 >= (length ys)

union' :: Set345 -> Set345 -> Set345
union' EmptySet EmptySet = EmptySet
union' (NonEmptySet xs) EmptySet = (NonEmptySet xs)
union' EmptySet (NonEmptySet ys) = (NonEmptySet ys)
union' (NonEmptySet (xs)) (NonEmptySet ys) = NonEmptySet (xs ++ helpunion' (NonEmptySet (xs)) (NonEmptySet ys))

helpunion' :: Set345 -> Set345 -> [Int] 
helpunion' (NonEmptySet []) (NonEmptySet ys) = []
helpunion' (NonEmptySet xs) (NonEmptySet []) = []
helpunion' (NonEmptySet (xs)) (NonEmptySet (y:ys))
   | elem y xs = helpunion' (NonEmptySet xs) (NonEmptySet ys)
   | otherwise = y : helpunion' (NonEmptySet xs) (NonEmptySet ys)

intersection' :: Set345 -> Set345 -> Set345
intersection' EmptySet _ = EmptySet
intersection' (NonEmptySet xs) EmptySet = EmptySet
intersection' (NonEmptySet (x:xs)) (NonEmptySet ys)
   | (helpintersection' (NonEmptySet (x:xs)) (NonEmptySet ys)) == [] = EmptySet
   | otherwise = NonEmptySet (helpintersection' (NonEmptySet (x:xs)) (NonEmptySet ys))
 
helpintersection' :: Set345 -> Set345 -> [Int] 
helpintersection' (NonEmptySet []) (NonEmptySet ys) = []
helpintersection' (NonEmptySet (x:xs)) (NonEmptySet ys)
   | elem x ys = x : helpintersection' (NonEmptySet xs) (NonEmptySet ys)
   | otherwise = helpintersection' (NonEmptySet xs) (NonEmptySet ys)

{----------}

countLetters :: IO [Int]
countLetters = do str1 <- getLine 
                  str2 <- getLine
                  str3 <- getLine
                  return [length str1, length str2, length str3]

and' :: Bool -> IO Bool 
and' True = do b <- getLine
               return (b == "True")
and' False = do c <- getLine
                return (False)


{----------}

data Tree1 = Leaf1 Int
           | Node1 Tree1 Int Tree1

t1 :: Tree1
t1 = Node1 (Leaf1 1) 2 (Leaf1 3)

preorder :: Tree1 -> [Int]
preorder (Leaf1 n) = [n]
preorder (Node1 x y z) = [y] ++ (preorder x) ++ (preorder z)

sumPositives :: Tree1 -> Int
sumPositives (Leaf1 n)
   | n >= 0       = n
   | otherwise    = 0
sumPositives (Node1 x y z) 
   | y >= 0       = y + (sumPositives x) + (sumPositives z)
   | otherwise    = (sumPositives x) + (sumPositives z)

countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 n)
   | n >= 0       = 0
   | otherwise    = 0
countInteriorNodes (Node1 x y z) 
   | y >= 0 || y < 0 = 1 + (countInteriorNodes x) + (countInteriorNodes z)
   | otherwise       = (countInteriorNodes x) + (countInteriorNodes z)

depth :: Tree1 -> Int
depth (Leaf1 n)
   | n >= 0       = 0
   | otherwise    = 0
depth (Node1 x y z) 
   | y >= 0     = 1 + (depth x) + (depth z)
   | otherwise  = (depth x) + (depth z)

balanced :: Tree1 -> Bool
balanced (Leaf1 n) = True
balanced (Node1 x y z) = abs ((helpbalanced x) - (helpbalanced z)) <= 1

helpbalanced :: Tree1-> Int
helpbalanced (Leaf1 n) = 0
helpbalanced (Node1 x y z) = (helpbalanced x) + (helpbalanced z) + 1