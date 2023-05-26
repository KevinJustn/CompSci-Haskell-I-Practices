-- This program is the basic "Hello World!" of Haskell.
-- There are two ways to run haskell files, 
-- compiling with ghc -o "compiled file name" "file name" and running haskell files, use "./'compiled file name'" if there is a main method
-- or "ghci 'file name'" and running methods in the prompt
-- to exit ghci, use "control" and "Y" (on mac)

main :: IO ()
main = putStrLn "Hello World!"