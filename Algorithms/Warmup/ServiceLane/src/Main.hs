module Main(main) where

import Control.Monad (replicateM, liftM)
import Data.Array (Array, listArray, (!))

readInts :: String -> [Int]
readInts line = map read $ words line

readTwoInts :: String -> (Int, Int)
readTwoInts line = (i1, i2)
    where i1:[i2] = readInts line

largestWidth :: Array Int Int -> (Int, Int) -> Int
largestWidth arr (i,j) = largestWidthAux (i,j) 3
    where largestWidthAux (li,lj) minWidth =
            if minWidth <= 1 || li>lj 
                then minWidth 
                else largestWidthAux (li+1, lj-1) minW 
            where startW = arr!li
                  endW = arr!lj
                  minW = min (min startW endW) minWidth

main :: IO ()
main = do
    (segments, tests) <- liftM readTwoInts getLine
    widths <- liftM (listArray (0, segments - 1) . take segments . readInts) getLine
    ijs <- liftM (map readTwoInts) $ replicateM tests getLine
    let solution = map (largestWidth widths) ijs
    mapM_ print solution