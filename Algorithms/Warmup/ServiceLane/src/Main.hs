module Main(main) where

import Data.Functor ((<$>))
import Control.Monad (replicateM)
import Data.Array ((!), Array, listArray)
import Data.List (partition) 

readInts :: String -> [Int]
readInts line = map read $ words line

readTwoInts :: String -> (Int, Int)
readTwoInts line = (i1, i2)
    where i1:[i2] = readInts line

largestWidth :: Array Int Int -> (Int, Int) -> Int
largestWidth arr (i,j) = minimum (wide ++ take 1 narrow)
    where (wide, narrow) = partition (>1) [arr!k | k <- [i..j]]    

main :: IO ()
main = do
    (segments, tests) <- readTwoInts <$> getLine
    widths <- listArray (0, segments - 1) . take segments . readInts <$> getLine
    ijs <- map readTwoInts <$> replicateM tests getLine
    let solution = map (largestWidth widths) ijs
    mapM_ print solution
          