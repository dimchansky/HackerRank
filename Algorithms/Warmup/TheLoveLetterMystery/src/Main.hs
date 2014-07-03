module Main(main) where

import Data.Function (on)
import Control.Monad (replicateM)
import Data.Char (ord)

charsDistance :: Char -> Char -> Int
charsDistance = ((abs.) . (-)) `on` ord

getPalindromeOps :: String -> Int
getPalindromeOps str = sum . take half_len $ zipWith charsDistance str (reverse str)
    where len = length str
          half_len = len `div` 2

main :: IO ()
main = mapM_ (print . getPalindromeOps) =<<
  flip replicateM getLine =<< readLn