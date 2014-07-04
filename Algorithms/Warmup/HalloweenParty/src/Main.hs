module Main where

import Control.Monad (replicateM)

-- v + h = k, maximize v * h = maximize (k - h) * h = maximize k*h - h^2
-- d(k*h - h^2)/dh = k - 2*h = 0
-- k = 2 * h
-- h = k / 2, v = k - h
maximumChocolatePieces :: Integer -> Integer
maximumChocolatePieces k = v * h
    where v = k `div` 2
          h = k - v

main :: IO ()
main = 
    readLn >>=
    flip replicateM readLn >>=
    mapM_ (print . maximumChocolatePieces)