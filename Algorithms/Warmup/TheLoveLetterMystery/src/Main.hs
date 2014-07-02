module Main(main) where

import Data.Function (on)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Char (ord)

charsDistance :: Char -> Char -> Int
charsDistance = ((abs.) . (-)) `on` ord

getPalindromeOps :: String -> Int
getPalindromeOps str = sum . take half_len $ zipWith charsDistance str (reverse str)
    where len = length str
          half_len = len `div` 2

main :: IO ()
main = do
    tests <- readLn
    testWords <- replicateM tests getLine
    mapM_ print $ getPalindromeOps <$> testWords