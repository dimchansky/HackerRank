module Main(main) where

import Control.Monad (replicateM)

utopianTreeHeight :: Int -> Integer
utopianTreeHeight c = foldl (flip ($)) 1 ops
    where ops = take c . concat . repeat $ [(*) 2, (+) 1]

main :: IO ()
main = do
    n <- readLn
    cycles <- replicateM n readLn
    let heights = map utopianTreeHeight cycles 
    mapM_ print heights