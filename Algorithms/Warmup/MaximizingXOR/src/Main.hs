module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Bits (xor)

maxXor :: Int -> Int -> Int
maxXor l r = maximum [a `xor` b | b <- [l..r], a <- [l..b]]

main :: IO ()
main = print =<< maxXor <$> readLn <*> readLn
    
