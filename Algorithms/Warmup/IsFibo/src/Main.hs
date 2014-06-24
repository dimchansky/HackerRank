module Main(main) where

import Control.Monad (replicateM)

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = sq * sq == n
    where sq = truncate $ sqrt $ (fromIntegral n :: Double)

-- A positive integer n is a Fibonacci number if and only if one or both of 5x^2+4 or 5x^2-4 is a perfect square.
isFibonacciNumber :: Integer -> Bool
isFibonacciNumber n = isPerfectSquare(fiveNSqr + 4) || isPerfectSquare(fiveNSqr - 4)
    where fiveNSqr = 5 * n * n

showIsFibo :: Bool -> String
showIsFibo True = "IsFibo"
showIsFibo False = "IsNotFibo"

main :: IO ()
main = do
    numberOfTestCases <- readLn
    tests <- replicateM numberOfTestCases readLn 
    let isFibo = map (showIsFibo . isFibonacciNumber) tests
    mapM_ putStrLn isFibo
