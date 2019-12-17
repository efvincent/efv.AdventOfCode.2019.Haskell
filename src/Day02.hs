module Day02 where

import Data.List (splitAt)
import AdventData (day02)

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
    (before, _:after) -> before ++ e : after
    _ -> xs

-- instructions say to replace posistions 1 and 2
memory = replace 1 12 (replace 2 2 day02)    

apply :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int] -> [Int]
apply f p1 p2 pAns mem =
    replace pAns (f v1 v2) mem
    where
        v1 = mem !! p1
        v2 = mem !! p2

run :: Int -> [Int] -> [Int]
run pos mem = case drop pos mem of
    (1:p1:p2:pAns:_) -> run (pos + 4) (apply (+) p1 p2 pAns mem)
    (2:p1:p2:pAns:_) -> run (pos + 4) (apply (*) p1 p2 pAns mem)
    _ -> mem

ans = 0
