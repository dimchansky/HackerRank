module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)

possibleValuesForLastStone :: Integer -> Integer -> Integer -> [Integer]
possibleValuesForLastStone n a b
    | a == b     = [(n - 1) * a] 
    | a >= b     = [na * a + (n - 1 - na) * b | na <- [0 .. n-1]]
    | otherwise = possibleValuesForLastStone n b a     

main :: IO ()
main = 
    readLn 
    >>= flip replicateM (possibleValuesForLastStone <$> readLn <*> readLn <*> readLn)
    >>= mapM_ (putStrLn . unwords . map show)
