module Main(main) where

import Control.Monad (replicateM)
import Data.Array (accumArray, Array, assocs)

checkElements :: String -> Array Char Bool
checkElements xs = accumArray (||) False ('a','z') (zip xs (repeat True))

uniqueElements :: String -> String
uniqueElements = map fst . filter snd . assocs . checkElements 

countElements :: String -> Array Char Int
countElements xs = accumArray (+) 0 ('a','z') (zip xs (repeat 1)) 

gemStones :: Int -> [String] -> Int
gemStones cnt rs = length . filter ((== cnt).snd) . assocs $ elementsOccurs
    where elementsOccurs = countElements . concatMap uniqueElements $ rs

main :: IO ()
main = do 
    rocksCount <- readLn
    rocks <- replicateM rocksCount getLine
    print . gemStones rocksCount $ rocks