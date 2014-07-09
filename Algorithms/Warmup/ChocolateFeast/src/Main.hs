module Main where

import Control.Monad (replicateM)

howManyChocolates :: Int -> Int -> Int -> Int
howManyChocolates n c m = count n 0 0
    where count money wrappers chocolates 
                | money < c && wrappers < m = chocolates
                | money >= c = let (chocolatesAdd, moneyLeft) = money `divMod` c 
                               in count moneyLeft (wrappers + chocolatesAdd) (chocolates + chocolatesAdd)
                | otherwise  = let (chocolatesAdd, wrappersLeft) = wrappers `divMod` m
                               in count money (wrappersLeft + chocolatesAdd) (chocolates + chocolatesAdd)
  
main :: IO ()
main =  readLn >>=
        flip replicateM getLine >>= 
        mapM_ (print . readLineTo howManyChocolates)
    where   
        readInts = map read . words
        readLineTo f s = f a b c
            where [a, b, c] = readInts s