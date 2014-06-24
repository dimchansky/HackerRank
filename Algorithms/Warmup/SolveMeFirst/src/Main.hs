module Main(main) where

solveMeFirst :: Integer -> Integer -> Integer
solveMeFirst a b = a + b

main :: IO ()
main = do
    val1 <- readLn
    val2 <- readLn
    let s = solveMeFirst val1 val2
    print s